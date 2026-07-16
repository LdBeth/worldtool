;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Frozen reference-world data (record / replay).
;;;
;;; The cold-load generator reads the unpatched distribution world through
;;; exactly three entry points -- WORLD-Q, WORLD-FIND-SYMBOLS and
;;; WORLD-SYMBOL-HOMES (everything else in wdecode.lisp is layered on
;;; WORLD-Q) -- so a build can be replayed without the .vlod present:
;;;
;;;   refrec   wraps the live world model and records every answer;
;;;   refdata  replays recorded answers; a datum that was never recorded
;;;            is a hard error naming the query, never a silent skip.
;;;
;;; `worldtool extract-reference` runs the coldtest + coldgen pipelines
;;; under a refrec and writes the recording as Lisp definitions
;;; (INSTALL-REFERENCE-DATA form); `--reference-data FILE` loads it back.
;;; A replay miss after a cold-set or gate change is expected behavior:
;;; re-extract against the distribution world and commit the new file.

(in-package #:worldtool)

(defstruct (refdata (:print-function
                     (lambda (rd stream depth)
                       (declare (ignore depth))
                       (format stream "#<refdata ~D Qs, ~D pnames, from ~A>"
                               (hash-table-count (refdata-qs rd))
                               (hash-table-count (refdata-homes rd))
                               (getf (refdata-provenance rd) :source)))))
  (provenance nil)                              ; plist, see WRITE-REFDATA
  (qs (make-hash-table))                        ; vma -> (tag . data), tag NIL = unmapped
  (symbol-vmas (make-hash-table :test #'equal)) ; pname -> vma list (may be NIL)
  (homes (make-hash-table :test #'equal))       ; pname -> home package names
  (aliases (make-hash-table :test #'equal)))    ; package alias -> primary name

(defstruct refrec
  model                                         ; live world-model being recorded
  (data (make-refdata)))

(defun copy-string-table (table)
  "Fresh EQUAL hash with TABLE's entries; list values are copied one level."
  (let ((copy (make-hash-table :test #'equal
                               :size (max 16 (hash-table-count table)))))
    (maphash (lambda (k v)
               (setf (gethash k copy) (if (consp v) (copy-list v) v)))
             table)
    copy))

;;; Replay

(defmethod world-q ((rd refdata) vma)
  (multiple-value-bind (entry found) (gethash vma (refdata-qs rd))
    (unless found
      (error "reference-data: Q #x~8,'0X was never recorded; re-run ~
`worldtool extract-reference` against the distribution world" vma))
    (if (car entry)
        (values (car entry) (cdr entry))
        (values nil nil))))

(defmethod world-find-symbols ((rd refdata) pname)
  (multiple-value-bind (vmas found) (gethash pname (refdata-symbol-vmas rd))
    (unless found
      (error "reference-data: symbol scan for ~S was never recorded; re-run ~
`worldtool extract-reference` against the distribution world" pname))
    (copy-list vmas)))

(defmethod world-symbol-homes ((rd refdata))
  (when (zerop (hash-table-count (refdata-homes rd)))
    (error "reference-data: no symbol-home oracle recorded; re-run ~
`worldtool extract-reference` against the distribution world"))
  ;; Fresh copies, mirroring the live path (which computes fresh tables per
  ;; call): the caller installs these as *COLD-SYMBOL-HOMES* /
  ;; *COLD-PACKAGE-ALIASES* and COLD-BUILD-PACKAGE-GRAPH then augments the
  ;; aliases with pkgdcl nicknames -- that must not mutate the frozen data.
  (values (copy-string-table (refdata-homes rd))
          (copy-string-table (refdata-aliases rd))))

;;; Record

(defmethod world-q ((rr refrec) vma)
  (multiple-value-bind (tag data) (world-q (refrec-model rr) vma)
    (setf (gethash vma (refdata-qs (refrec-data rr))) (cons tag data))
    (values tag data)))

(defmethod world-find-symbols ((rr refrec) pname)
  (let ((vmas (world-find-symbols (refrec-model rr) pname)))
    (setf (gethash pname (refdata-symbol-vmas (refrec-data rr))) vmas)
    vmas))

(defmethod world-symbol-homes ((rr refrec))
  (multiple-value-bind (homes aliases) (world-symbol-homes (refrec-model rr))
    ;; Snapshot: the caller mutates the returned tables (pkgdcl nickname
    ;; augmentation, cold-pkg.lisp); the recording must keep the reference
    ;; world's own answer.
    (setf (refdata-homes (refrec-data rr)) (copy-string-table homes)
          (refdata-aliases (refrec-data rr)) (copy-string-table aliases))
    (values homes aliases)))

;;; Serialization.  The generated file is one INSTALL-REFERENCE-DATA form;
;;; entries are sorted (Qs by vma, string tables by key) so re-extraction
;;; diffs minimally, but the per-pname home list keeps its recorded order --
;;; COLD-SYMBOL-HOME's tie-breaks read meaning into it.

(defvar *reference-data* nil
  "Set by INSTALL-REFERENCE-DATA when a generated reference file is loaded.")

(defun install-reference-data (&key provenance homes aliases symbol-vmas qs)
  (let ((rd (make-refdata :provenance provenance)))
    (dolist (h homes) (setf (gethash (first h) (refdata-homes rd)) (rest h)))
    (dolist (a aliases) (setf (gethash (car a) (refdata-aliases rd)) (cdr a)))
    (dolist (s symbol-vmas)
      (setf (gethash (first s) (refdata-symbol-vmas rd)) (rest s)))
    (dolist (q qs)
      (setf (gethash (first q) (refdata-qs rd))
            (cons (second q) (third q))))
    (setf *reference-data* rd)))

(defun load-refdata (path)
  "Load a generated reference-data file; returns the refdata it installs."
  (let ((*reference-data* nil))
    (load path)
    (or *reference-data*
        (error "~A did not install reference data" path))))

(defun universal-time-string (ut)
  (multiple-value-bind (sec min hour day month year) (decode-universal-time ut 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

(defun write-refdata (rd path &key source)
  "Write RD to PATH as a loadable INSTALL-REFERENCE-DATA form."
  (let ((provenance
          (list :source (namestring (truename source))
                :bytes (with-open-file (s source :element-type '(unsigned-byte 8))
                         (file-length s))
                :write-date (universal-time-string (file-write-date source))
                :extracted (universal-time-string (get-universal-time)))))
    (flet ((sorted-keys (table)
             (sort (loop for k being the hash-keys of table collect k)
                   (if (eq (hash-table-test table) 'equal) #'string< #'<))))
      (with-open-file (f path :direction :output :if-exists :supersede)
        (format f ";;; -*- Mode: Lisp; Package: WORLDTOOL -*-~%~
;;; Generated by `worldtool extract-reference`.  Do not edit.~%~
;;; Every datum the coldtest + coldgen pipelines read from the~%~
;;; distribution world; load via --reference-data in place of --reference.~%~
\(in-package #:worldtool)~2%~
\(install-reference-data~% :provenance '~S~% :homes~% '(" provenance)
        (dolist (pname (sorted-keys (refdata-homes rd)))
          (format f "~%   (~S~{ ~S~})" pname (gethash pname (refdata-homes rd))))
        (format f ")~% :aliases~% '(")
        (dolist (name (sorted-keys (refdata-aliases rd)))
          (format f "~%   (~S . ~S)" name (gethash name (refdata-aliases rd))))
        (format f ")~% :symbol-vmas~% '(")
        (dolist (pname (sorted-keys (refdata-symbol-vmas rd)))
          (format f "~%   (~S~{ #x~8,'0X~})"
                  pname (gethash pname (refdata-symbol-vmas rd))))
        (format f ")~% :qs~% '(")
        (dolist (vma (sorted-keys (refdata-qs rd)))
          (destructuring-bind (tag . data) (gethash vma (refdata-qs rd))
            (if tag
                (format f "~%   (#x~8,'0X #x~2,'0X #x~8,'0X)" vma tag data)
                (format f "~%   (#x~8,'0X)" vma))))
        (format f "))~%")))
    path))
