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

;;; ---------------- Cell-reference constant snapping ----------------

(defvar *cold-snap-cell-refs* t
  "When true (the default), a compiled-code constant that names a CELL --
an EVCP (SYMEVAL/PUSH-INDIRECT) or a DTP-LOCATIVE -- is emitted pointing
at the FINAL cell of the one-q-forward chain, not at the forwarded cell
the compiler named.  This is what stock Genera's loader does: it snaps
invisible pointers when it builds compiled-code constants, so a compiled
reference to a wired special variable resolves to the WIRED cell.

Without the snap the constant points at the heap symbol's value cell,
which DECLARE-STORAGE-CATEGORY-LOAD (COLD-DO-DSCL, cold-eval.lisp) has
overwritten with a DTP-ONE-Q-FORWARD into the wired symbol-cell table.
Reads still work -- the microcode follows the forward -- but a raw
%MEMORY-WRITE through the unsnapped DTP-LOCATIVE constant clobbers the
FORWARDING POINTER ITSELF, splitting the variable into two cells that
then drift apart.  Proven instance: SI:*INTERRUPT-TASK-FREE-LIST* in
(INTERNAL ENQUEUE-INTERRUPT-TASK 0 ENQUEUE-INTERRUPT-TASK-INTERNAL),
whose C4/D9 constants named the heap cell #x8011287D (holding
05:F8042739) while the distribution world names #xF8042625 directly.
The desync corrupts the interrupt-task queue seconds into every boot and
eventually kills QLD with \"Interrupt task queue is full\".

NIL exists for the gate negative test only (CHECK-CELL-REF-SNAPPING;
`worldtool coldtest ... --defeat snap-cell-refs').

Out of scope, deliberately: DTP-CALL-INDIRECT constants (tag byte AA)
naming function cells.  Stock also links those to the callee's CCA once
it is defined; doing so here is a separate change with its own gate.")

(defparameter *cold-code-operand-patch-tags* t
  "T: a first-boot patch into a compiled-code operand slot whose tag came
from the INSTRUCTION (op bit 8, TFT) carries that tag byte, and
cold-finalize emits (SYS:%SET-TAG value tagbyte) so %P-STORE-CONTENTS --
which writes a COMPLETE Q -- cannot retype the slot to whatever the
boot-time value happens to be tagged.

NIL exists for the gate negative test only
(CHECK-CODE-OPERAND-PATCH-TAGS; `worldtool coldtest ... --defeat
code-operand-patch-tags').  Defeated, the generic-function operands go
back to being retyped DTP-LIST by FLAVOR:FIND-GENERIC-FUNCTION-AS-
CONSTANT's cold FSET stub -- QLD attempt 21, guest trap 46 in
FUNCTION-SPEC-DEFAULT-HANDLER.

The flag deliberately does NOT reach non-TFT patches: there the baked
tag came from the PLACEHOLDER value's own type, and forcing it onto the
real boot-time value would be a fresh bug of the same family.")

(defun cold-cell-ref-type-p (w type)
  "True for the constant tag types that name a CELL and therefore get
snapped through one-q-forwards: EVCP and DTP-LOCATIVE."
  (or (= type (cold-dtp w "EXTERNAL-VALUE-CELL-POINTER"))
      (= type (cold-dtp w "LOCATIVE"))))

(defun cold-resnap-cell-refs (w)
  "Re-snap every recorded compiled-code cell reference (M3h: the
ordering hazard).  A CCA can be materialized BEFORE the cell it names is
forwarded -- vbins load in file order, so a cross-file reference to a
wired variable routinely sees the plain heap cell and COLD-FUN's
emit-time snap is a no-op.  This pass runs after the last vbin, walks the
recorded Q sites and re-follows each chain.  It only rewrites the DATA
word of Qs that already exist; it allocates nothing, so it is safe to run
after the last allocating finalize step and before the region
free-pointer re-stamp.  Returns the number of Qs changed."
  (let ((changed 0))
    (dolist (vma (cold-world-cell-ref-sites w) changed)
      (multiple-value-bind (tag data present) (cw-ref w vma)
        (when (and present (cold-cell-ref-type-p w (tag-type tag)))
          (let ((final (cold-follow-cell w data)))
            (unless (eql final data)
              (cw-set w vma tag final)
              (incf changed))))))))

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
                  (let ((tagbyte (if tft
                                     (ldb (byte 8 0) op)
                                     (logior (logand op #xC0)
                                             (tag-type vtag)))))
                    ;; A constant that names a CELL is snapped through
                    ;; one-q-forwards, as stock Genera's loader does.  The
                    ;; tag byte (cdr bits included) is untouched; only the
                    ;; data word moves to the final cell.  The site is
                    ;; recorded because the forward may not exist yet --
                    ;; cold-resnap-cell-refs revisits it at finalize.
                    (when (and *cold-snap-cell-refs*
                               (cold-cell-ref-type-p w (tag-type tagbyte)))
                      (push (+ fn i) (cold-world-cell-ref-sites w))
                      (setf vdata (cold-follow-cell w vdata)))
                    (cw-set w (+ fn i) tagbyte vdata)
                    ;; A load-time-eval operand the mini-eval could not value
                    ;; leaves a first-boot patch request for the Q just
                    ;; stored.  TFT (op bit 8) means the tag byte came from
                    ;; the INSTRUCTION -- a call-kind operand slot -- so the
                    ;; patch must re-stamp it; %P-STORE-CONTENTS would
                    ;; otherwise retype the slot to the boot value's own type
                    ;; (QLD attempt 21, trap 46).  When TFT is false the tag
                    ;; was derived from the PLACEHOLDER value's type instead,
                    ;; and forcing it onto the real value would be a new bug
                    ;; of the same shape -- leave those patches untagged.
                    (when *cold-eval-patch-form*
                      (cold-note-patch w (+ fn i)
                                       (shiftf *cold-eval-patch-form* nil)
                                       (and *cold-code-operand-patch-tags*
                                            tft tagbyte))))))))
          fn))))

(defun cold-fun-entry-pc (w fn &key pc-to-entry-p)
  "The PC SET-TRAP-VECTOR-ENTRY stores for the function at FN."
  (if pc-to-entry-p
      fn
      (multiple-value-bind (tag data) (cw-ref w fn)
        (declare (ignore tag))
        (+ fn 1 (- (ldb (byte 8 18) data) (ldb (byte 8 0) data))))))
