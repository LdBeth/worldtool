;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Command-line driver (invoked via the worldtool shell wrapper).

(in-package #:worldtool)

(defun parse-qs-arg (s)
  "Parse PAGE:COUNT."
  (let ((colon (position #\: s)))
    (if colon
        (cons (parse-integer s :end colon)
              (parse-integer s :start (1+ colon)))
        (cons (parse-integer s) 1))))

(defun parse-vma-arg (s)
  "Parse HEXADDR[:COUNT]."
  (let ((colon (position #\: s)))
    (if colon
        (cons (parse-integer s :end colon :radix 16)
              (parse-integer s :start (1+ colon)))
        (cons (parse-integer s :radix 16) 1))))

(defun usage ()
  (format t "usage: worldtool dump FILE [--qs PAGE:COUNT]~%~
             ~7T worldtool inspect FILE LAYOUT.sexp [--vma HEXADDR[:COUNT]]~%~
             ~7T worldtool symbols FILE [--min N]~%~
             ~7T worldtool symval FILE PNAME~%~
             ~7T worldtool vbin FILE... [--trace]~%~
             ~7T worldtool export FILE OUT.sexp OUT.qs~%~
             ~7T worldtool emit SPEC.sexp OUT~%~
             ~7T worldtool roundtrip FILE~%~
             ~7T worldtool coldtest LAYOUT.sexp TMPDIR [--reference WORLD | ~
--reference-data FILE]~%~
             ~7T worldtool coldgen LAYOUT.sexp OUT.ilod (--reference WORLD | ~
--reference-data FILE) [--sys SYSDIR]~%~
             ~7T worldtool extract-reference LAYOUT.sexp WORLD OUT.lisp ~
TMPDIR [--sys SYSDIR]~%~
             ~7T   --sys defaults to $GENERA_SYS_ROOT~%")
  1)

(defun main (args)
  (handler-case
      (cond
        ((null args) (usage))
        ((string= (first args) "dump")
         (let ((file (second args))
               (qs (let ((p (position "--qs" args :test #'string=)))
                     (and p (parse-qs-arg (nth (1+ p) args))))))
           (unless file (return-from main (usage)))
           (dump-world file :qs qs)
           0))
        ((string= (first args) "inspect")
         (let ((file (second args))
               (layout (third args))
               (vma (let ((p (position "--vma" args :test #'string=)))
                      (and p (parse-vma-arg (nth (1+ p) args))))))
           (unless (and file layout) (return-from main (usage)))
           (inspect-world file layout :vma vma)
           0))
        ((string= (first args) "symbols")
         (let ((file (second args))
               (min (let ((p (position "--min" args :test #'string=)))
                      (if p (parse-integer (nth (1+ p) args)) 4))))
           (unless file (return-from main (usage)))
           (symbols-world file :min min)
           0))
        ((string= (first args) "symval")
         (let ((file (second args))
               (pname (third args)))
           (unless (and file pname) (return-from main (usage)))
           (let ((model (read-world file)))
             (multiple-value-bind (value vma n)
                 (world-symbol-value model pname)
               (format t "~&~A (symbol #x~8,'0X~@[, ~D candidates~]): ~S~%"
                       pname vma (and (> n 1) n) value)))
           0))
        ((string= (first args) "vbin")
         (let* ((rest (rest args))
                (trace (member "--trace" rest :test #'string=))
                (files (remove "--trace" rest :test #'string=)))
           (unless files (return-from main (usage)))
           (if (vbin-world files :trace (and trace t)) 0 1)))
        ((string= (first args) "export")
         (destructuring-bind (file sexp qs) (rest args)
           (export-world file sexp qs)
           (format t "~&exported ~A -> ~A + ~A~%" file sexp qs)
           0))
        ((string= (first args) "emit")
         (destructuring-bind (spec out) (rest args)
           (emit-world spec out)
           (format t "~&emitted ~A -> ~A~%" spec out)
           0))
        ((string= (first args) "roundtrip")
         (if (roundtrip (second args)) 0 1))
        ((string= (first args) "coldgen")
         (let ((layout (second args))
               (out (third args))
               (reference (let ((p (position "--reference" args :test #'string=)))
                            (and p (nth (1+ p) args))))
               (refdata (let ((p (position "--reference-data" args
                                           :test #'string=)))
                          (and p (nth (1+ p) args))))
               (sysdir (let ((p (position "--sys" args :test #'string=)))
                         (and p (nth (1+ p) args)))))
           ;; SYSDIR may be omitted; SETUP-SYS-HOST then falls back to
           ;; GENERA_SYS_ROOT, and errors clearly if that is unset too.
           (unless (and layout out (or reference refdata))
             (return-from main (usage)))
           (coldgen layout out :reference reference :reference-data refdata
                    :sysdir sysdir)))
        ((string= (first args) "coldtest")
         (let ((layout (second args))
               (tmpdir (third args))
               (reference (let ((p (position "--reference" args :test #'string=)))
                            (and p (nth (1+ p) args))))
               (refdata (let ((p (position "--reference-data" args
                                           :test #'string=)))
                          (and p (nth (1+ p) args))))
               (sysdir (let ((p (position "--sys" args :test #'string=)))
                         (and p (nth (1+ p) args)))))
           (unless (and layout tmpdir) (return-from main (usage)))
           (if (zerop (cold-test (pathname tmpdir)
                                 :layout-path layout :reference reference
                                 :reference-data refdata :sysdir sysdir))
               0 1)))
        ((string= (first args) "extract-reference")
         (let ((layout (second args))
               (world (third args))
               (out (fourth args))
               (tmpdir (fifth args))
               (sysdir (let ((p (position "--sys" args :test #'string=)))
                         (and p (nth (1+ p) args)))))
           (unless (and layout world out tmpdir)
             (return-from main (usage)))
           (extract-reference layout world out tmpdir :sysdir sysdir)))
        (t (usage)))
    (error (e)
      (format *error-output* "~&worldtool: ~A~%" e)
      2)))
