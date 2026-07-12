;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Extract the Lisp symbol/string print-names embedded in a world's data
;;; pages.  Two kinds live there:
;;;
;;;   1. Genuine Ivory character arrays: a header Q (Type_HeaderI/HeaderP,
;;;      element-type Character) followed by ceil(len/per-word) data Qs, each
;;;      packing per-word characters low-byte-first.  We decode these exactly.
;;;
;;;   2. Print-names packed into fixnum arrays -- the representation used by
;;;      the debug-info tables (gc/debug-info.lisp) that make up the bulk of
;;;      the VLM debugger world.  These are not individually headered, so we
;;;      recover them by scanning consecutive immediate (fixnum/single-float)
;;;      Qs and reading their data words as 4 low-byte-first ASCII bytes.
;;;
;;; Both decode char 0 into the low byte of the data word.  A plain `strings`
;;; pass fails here because the 4 tag bytes are interleaved every 4 Qs (20-byte
;;; Ivory groups); walking Qs skips the tag bytes and fences runs on the header
;;; and pointer Qs that separate one name from the next.

(in-package #:worldtool)

(declaim (inline printing-char-p))
(defun printing-char-p (byte)
  "Printable, space excluded -- spaces don't occur inside packed print-names
and letting them through would glue adjacent runs."
  (<= 33 byte 126))

(defun immediate-data-q-p (tag)
  "True for Qs whose data word holds packed immediate bits (fixnum /
single-float); these carry the character bytes of packed print-names."
  (let ((type (tag-type tag)))
    (or (= type +type-fixnum+) (= type +type-single-float+))))

(defun decode-char-array (qv i)
  "If Q I of QV heads a printable Character array, return (values STRING
NEXT-INDEX); otherwise (values NIL (1+ I))."
  (multiple-value-bind (tag data) (qref qv i)
    (let ((type (tag-type tag)))
      (when (or (= type +type-header-i+) (= type +type-header-p+))
        (let ((elt (ldb (byte 2 30) data))
              (packing (ldb (byte 3 27) data))
              (len (ldb (byte +array-length-bits+ 0) data)))
          (when (and (= elt +array-element-type-character+)
                     (not (logbitp +array-long-prefix-bit+ data)) ; inline LEN only
                     (member packing '(0 1 2))     ; 32/16/8-bit characters
                     (plusp len) (<= len 1024))
            (let* ((per-word (ash 1 packing))
                   (bits (ash 32 (- packing)))
                   (words (ceiling len per-word)))
              (when (<= (+ i 1 words) (qvec-length qv))
                (let ((s (make-string len)) (ok t))
                  (dotimes (k len)
                    (multiple-value-bind (dt dd) (qref qv (+ i 1 (floor k per-word)))
                      (declare (ignore dt))
                      (let ((c (ldb (byte bits (* bits (mod k per-word))) dd)))
                        (cond ((and (< c 128) (or (printing-char-p c) (= c 32)))
                               (setf (char s k) (code-char c)))
                              (t (setq ok nil) (return))))))
                  (when ok
                    (return-from decode-char-array
                      (values s (+ i 1 words))))))))))
      (values nil (1+ i)))))

(defun map-packed-runs (qv min fn)
  "Call FN on each maximal run of >= MIN printable characters packed into
consecutive immediate Qs of QV."
  (let ((run (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)))
    (flet ((flush ()
             (when (>= (fill-pointer run) min)
               (funcall fn (subseq run 0 (fill-pointer run))))
             (setf (fill-pointer run) 0)))
      (dotimes (i (qvec-length qv))
        (multiple-value-bind (tag data) (qref qv i)
          (if (immediate-data-q-p tag)
              (dotimes (b 4)
                (let ((c (ldb (byte 8 (* 8 b)) data)))
                  (if (printing-char-p c)
                      (vector-push-extend (code-char c) run)
                      (flush))))
              (flush))))
      (flush))))

(defun collect-symbols (model &key (min 4))
  "Walk every data-pages payload of MODEL.  Returns (values TYPED PACKED):
TYPED is the sorted set of print-names backed by a real Character array;
PACKED is the sorted set of the remaining fixnum-packed runs (>= MIN chars)."
  (let ((typed (make-hash-table :test #'equal))
        (packed (make-hash-table :test #'equal)))
    (dolist (e (append (world-model-wired-map model)
                       (world-model-unwired-map model)))
      (when (and (= (map-entry-opcode e) +op-data-pages+) (map-entry-payload e))
        (let* ((qv (map-entry-payload e)) (n (qvec-length qv)) (i 0))
          (loop while (< i n) do
            (multiple-value-bind (s next) (decode-char-array qv i)
              (when (and s (>= (length s) min))
                (setf (gethash s typed) t))
              (setf i next)))
          (map-packed-runs qv min
                           (lambda (s)
                             (unless (gethash s typed)
                               (setf (gethash s packed) t)))))))
    (flet ((sorted (h) (sort (loop for k being the hash-keys of h collect k)
                             #'string<)))
      (values (sorted typed) (sorted packed)))))

(defun symbols-world (path &key (min 4) (stream *standard-output*))
  "Print the print-names embedded in world PATH's data pages."
  (let ((model (read-world path)))
    (multiple-value-bind (typed packed) (collect-symbols model :min min)
      (format stream "~&~A: ~D character-array string~:P, ~
                      ~D packed print-name run~:P (>= ~D chars)~%"
              path (length typed) (length packed) min)
      (when typed
        (format stream "~%character arrays (~D):~%" (length typed))
        (dolist (s typed) (format stream "  ~A~%" s)))
      (when packed
        (format stream "~%packed print-names (~D):~%" (length packed))
        (dolist (s packed) (format stream "  ~A~%" s))))
    model))
