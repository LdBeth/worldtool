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
             ~7T worldtool vbin FILE... [--trace]~%~
             ~7T worldtool export FILE OUT.sexp OUT.qs~%~
             ~7T worldtool emit SPEC.sexp OUT~%~
             ~7T worldtool roundtrip FILE~%")
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
        (t (usage)))
    (error (e)
      (format *error-output* "~&worldtool: ~A~%" e)
      2)))
