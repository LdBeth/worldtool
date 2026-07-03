;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: per-stage structural checks against ground truth.
;;;
;;; The reference world is the ORIGINAL UNPATCHED og2vlm/Genera-8-5.vlod
;;; whose annotated dump is committed as worldtool/genera-8-5-wired.txt.
;;; Checks compare STRUCTURE (tags, self-pointers, population), never raw
;;; addresses -- a fresh world lays objects out at different VMAs.

(in-package #:worldtool)

(defvar *cold-check-failures*)

(defun cold-check (ok fmt &rest args)
  (unless ok
    (push (apply #'format nil fmt args) *cold-check-failures*))
  ok)

(defmacro with-cold-checks ((name) &body body)
  `(let ((*cold-check-failures* nil))
     ,@body
     (cond (*cold-check-failures*
            (format t "~&~A: ~D check~:P FAILED~%" ,name
                    (length *cold-check-failures*))
            (dolist (f (reverse *cold-check-failures*))
              (format t "  FAIL ~A~%" f))
            nil)
           (t (format t "~&~A: OK~%" ,name)
              t))))

(defun check-symbol-block (w model vma what &key reference reference-vma)
  "The 5-Q symbol shape at VMA in the emitted MODEL, optionally against the
same block in a REFERENCE model (tag classes only; data only where it is a
self-pointer)."
  (let ((dtp-header-p (cold-dtp w "HEADER-P"))
        (dtp-null (cold-dtp w "NULL")))
    (multiple-value-bind (tag data) (world-q model vma)
      (declare (ignore data))
      (cold-check (and tag (= (tag-type tag) dtp-header-p))
                  "~A+0: header-p expected, got tag ~2,'0X" what tag))
    (multiple-value-bind (tag data) (world-q model (+ vma 2))
      (cold-check (and tag (= (tag-type tag) dtp-null) (= data vma))
                  "~A+2: dtp-null self expected, got ~2,'0X:~8,'0X" what tag data))
    (when reference
      (loop for offset in '(0 2)
            do (multiple-value-bind (gtag gdata) (world-q model (+ vma offset))
                 (declare (ignore gdata))
                 (multiple-value-bind (rtag rdata)
                     (world-q reference (+ reference-vma offset))
                   (declare (ignore rdata))
                   (cold-check (and gtag rtag (= (tag-type gtag) (tag-type rtag)))
                               "~A+~D: tag ~2,'0X vs reference ~2,'0X"
                               what offset gtag rtag)))))))

(defun check-skeleton (w model &key reference)
  "M3a gate: NIL/T shape, fully-populated trap page, comm pages present."
  (with-cold-checks ("cold skeleton")
    (let* ((layout (cold-world-layout w))
           (nil-vma (cold-world-nil-vma w))
           (t-vma (cold-world-t-vma w))
           (trap-base (cold-address w "%TRAP-VECTOR-BASE"))
           (trap-length (layout-value layout "%TRAP-VECTOR-LENGTH"))
           (even-pc (cold-dtp w "EVEN-PC")))
      (check-symbol-block w model nil-vma "NIL"
                          :reference reference :reference-vma nil-vma)
      (check-symbol-block w model t-vma "T"
                          :reference reference :reference-vma t-vma)
      ;; Every trap vector slot holds an even-pc into the catch-all.
      (let ((bad 0) (unwired 0))
        (dotimes (i trap-length)
          (multiple-value-bind (tag data) (world-q model (+ trap-base i))
            (cond ((null tag) (incf unwired))
                  ((not (and (= (tag-type tag) even-pc)
                             (= data (cold-world-catch-all-pc w))))
                   (incf bad)))))
        (cold-check (zerop unwired) "trap page: ~D unwired slots" unwired)
        (cold-check (zerop bad) "trap page: ~D non-catch-all slots" bad))
      ;; Communication pages exist (world-q non-NIL means mapped).
      (dolist (block (layout-section layout :magic-locations))
        (destructuring-bind (name start end ventries) block
          (declare (ignore ventries))
          (when (and (>= start #xF8000000) (< end +wired-zone-limit+))
            (cold-check (world-q model start)
                        "~A start #x~8,'0X not mapped" name start)
            (cold-check (world-q model (1- end))
                        "~A end #x~8,'0X not mapped" name (1- end)))))
      ;; The catch-all code block itself.
      (multiple-value-bind (tag data)
          (world-q model (cold-world-catch-all-pc w))
        (cold-check (and tag
                         (= (tag-type tag) (cold-dtp w "PACKED-INSTRUCTION-62"))
                         (= data #xF000BC00))
                    "catch-all: packed halt-halt expected, got ~2,'0X:~8,'0X"
                    tag data)))))

;;; Stage-test driver (CLI: worldtool coldtest TMPDIR [REFERENCE-WORLD])

(defun cold-test (tmpdir &key reference layout-path)
  "Build the current-stage cold world, emit, re-read, check.  Returns the
number of failed stages (0 = success)."
  (let* ((layout (read-layout layout-path))
         (failures 0)
         (w (make-skeleton-world layout))
         (out (format nil "~A/cold-skeleton.ilod" tmpdir))
         (model (cold-world-model w))
         (reference-model (when reference (read-world reference))))
    (write-file-bytes out (write-world model))
    (let ((reread (read-world out)))
      (unless (check-skeleton w reread :reference reference-model)
        (incf failures)))
    failures))
