;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: compiled-function materialization.
;;;
;;; Mirrors LOAD-I-COMPILED-FUNCTION (l-bin/load.lisp:941) and the IMACH
;;; MAKE-COMPILED-CODE (sys/lcode.lisp:825): the block is
;;;   [CCA header: dtp-header-i, cdr %HEADER-TYPE-COMPILED-FUNCTION,
;;;    data = suffix-size<<18 | total-size          (sysdef.lisp:726)]
;;;   [function cell: dtp-compiled-function -> CCA+2]
;;;   [total-size - 2 Qs: instructions, then constants/debug-info suffix]
;;; The vword opcode (l-bin wire format, validated by the decoder): bits
;;; 0-7 tag byte, bit 8 TYPE-FROM-TAG, bit 9 IMMEDIATE (32-bit datum sent
;;; low16/high16 -- dump.lisp:468 SEND-WORD), bit 10 RELATIVE (datum is an
;;; offset from the function address).
;;;
;;; Entry PC (for trap vectors, sys/iprim.lisp:61): functions taking
;;; &optional/&rest enter at the function address itself; otherwise skip
;;; the entry sequence: fn + 1 + (%%ENTRY-INSTRUCTION-MAX -
;;; %%ENTRY-INSTRUCTION-MIN), both byte fields of the entry instruction's
;;; data word.

(in-package #:worldtool)

(defun name-and-storage-entry (vfun name)
  "Entry named NAME in the vfun's (fspec . debug-alist) storage info."
  (loop for entry in (rest (vfun-name-and-storage vfun))
        when (and (consp entry) (vsym-p (first entry))
                  (string= (vsym-name (first entry)) name))
          return entry))

(defun vfun-storage-category (vfun)
  "(values :unsafeguarded/:safeguarded/:wired area-designator)"
  (let* ((sc (name-and-storage-entry vfun "STORAGE-CATEGORY"))
         (descriptor (if sc (second sc) 0))
         (category (case (ldb (byte 2 0) descriptor)
                     (0 :unsafeguarded) (1 :safeguarded) (2 :wired)
                     (t (error "Bad storage category in ~S"
                               (vfun-name-and-storage vfun))))))
    (values category
            (ecase category
              (:unsafeguarded
               (let ((area (name-and-storage-entry vfun "AREA")))
                 (if area
                     (let ((sym (second area)))
                       (unless (vsym-p sym)
                         (error "Bad COMPILER:AREA ~S" area))
                       (remove #\* (vsym-name sym)))
                     "COMPILED-FUNCTION-AREA")))
              (:wired "WIRED-CONTROL-TABLES")
              (:safeguarded "SAFEGUARDED-OBJECTS-AREA")))))

(defun cold-fun (w vfun)
  "Materialize VFUN; returns the function address (CCA+2)."
  (or (gethash vfun *cold-object-vmas*)
      (multiple-value-bind (category area) (vfun-storage-category vfun)
        (declare (ignore category))
        (let* ((total (vfun-total-size vfun))
               (suffix (vfun-suffix-size vfun))
               (cca (cold-alloc w area total))
               (fn (+ cca 2))
               (words (vfun-words vfun))
               (dtp-cf (cold-dtp w "COMPILED-FUNCTION")))
          (setf (gethash vfun *cold-object-vmas*) fn)
          (cw-set w cca
                  (tag (layout-value (cold-world-layout w)
                                     "SYSTEM:%HEADER-TYPE-COMPILED-FUNCTION")
                       (cold-dtp w "HEADER-I"))
                  (logior (ash suffix 18) total))
          (cw-set w (1+ cca) (tag 0 dtp-cf) fn)
          (let ((*cold-cca-base* cca))
            (dotimes (i (length words))
              (let* ((vw (aref words i))
                     (op (vword-op vw))
                     (tft (logbitp 8 op))
                     (imm (logbitp 9 op))
                     (rel (logbitp 10 op))
                     (datum (vword-data vw)))
                (multiple-value-bind (vtag vdata)
                    (cond (imm (values (tag 0 (cold-dtp w "FIXNUM"))
                                       (ldb (byte 32 0) datum)))
                          ((integerp datum)
                           (values (tag 0 (cold-dtp w "FIXNUM"))
                                   (ldb (byte 32 0) datum)))
                          (t (cold-ref w datum)))
                  (when rel
                    (unless (and (integerp datum) (<= 0 datum (- total 3)))
                      (error "Invalid relative operand ~S in ~S"
                             datum (first (vfun-name-and-storage vfun))))
                    (setf vdata (+ fn datum)))
                  (cw-set w (+ fn i)
                          (if tft
                              (ldb (byte 8 0) op)
                              (logior (logand op #xC0) (tag-type vtag)))
                          vdata)
                  ;; A load-time-eval operand the mini-eval could not value
                  ;; leaves a first-boot patch request for the Q just stored.
                  (when *cold-eval-patch-form*
                    (cold-note-patch w (+ fn i)
                                     (shiftf *cold-eval-patch-form* nil)))))))
          fn))))

(defun cold-fun-entry-pc (w fn &key pc-to-entry-p)
  "The PC SET-TRAP-VECTOR-ENTRY stores for the function at FN."
  (if pc-to-entry-p
      fn
      (multiple-value-bind (tag data) (cw-ref w fn)
        (declare (ignore tag))
        (+ fn 1 (- (ldb (byte 8 18) data) (ldb (byte 8 0) data))))))
