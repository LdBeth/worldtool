;;; Cold-set band audit (M3h boot 38, read-only analysis).
;;; For each source file: extract plain top-level (defun NAME ...) names,
;;; resolve each name's function cell in the DIST world, bucket the fcell
;;; data band (0x882 = cold-load CCA, 0x822/0x823 = QLD/system build),
;;; emit one verdict row per file.
;;;
;;; usage: sbcl --script coldset-audit.lisp DIST.vlod SYSDIR FILELIST.txt
;;;   FILELIST.txt: one "SYS: DIR; NAME" per line (blank/;-comment skipped).
;;; Env not needed.  Prints a table + a machine block per file.

(let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (f '("src/package" "src/constants" "src/image" "src/ilod"
               "src/vlod" "src/dump" "src/inspect" "src/symbols" "src/vbin"
               "src/emit" "src/wdecode"))
    (load (merge-pathnames (concatenate 'string f ".lisp") here))))
(in-package #:worldtool)

;;; ---- source path mapping ---------------------------------------------------

(defun sys-spec->path (sysdir spec &optional (type "lisp"))
  "\"SYS: DIR; SUB; NAME\" -> SYSDIR/dir/sub/name.type (lowercase)."
  (let* ((body (string-trim " " (subseq spec (1+ (position #\: spec)))))
         (parts (loop with s = 0
                      for semi = (position #\; body :start s)
                      collect (string-trim " " (subseq body s (or semi (length body))))
                      while semi do (setf s (1+ semi))))
         (dirs (butlast parts))
         (name (car (last parts))))
    (format nil "~A~{~A/~}~A.~A"
            (namestring (merge-pathnames "" sysdir))
            (mapcar #'string-downcase dirs)
            (string-downcase name) type)))

(defun file-package-name (path)
  "Read the -*- Package: X -*- attribute (uppercased) or NIL."
  (with-open-file (s path :if-does-not-exist nil)
    (when s
      (let ((line (read-line s nil "")))
        (let ((p (search "Package:" line :test #'char-equal)))
          (when p
            (let* ((start (+ p 8))
                   (end (position-if (lambda (c)
                                       (member c '(#\Space #\Tab #\; #\-)))
                                     line :start (position-if-not
                                                  (lambda (c) (member c '(#\Space #\Tab)))
                                                  line :start start))))
              (string-upcase
               (string-trim " " (subseq line start end))))))))))

(defun extract-defun-names (path)
  "Plain top-level DEFUN names: lines beginning (no indent) with \"(defun \"
followed by a plain symbol (not \"(setf\").  Returns uppercased strings."
  (let ((names nil))
    (with-open-file (s path :if-does-not-exist nil :external-format :latin-1)
      (when s
        (loop for line = (read-line s nil nil)
              while line do
          (when (and (>= (length line) 7)
                     (string= (subseq line 0 7) "(defun "))
            (let* ((rest (string-left-trim " " (subseq line 6)))
                   (name (with-input-from-string (in rest)
                           (let ((*read-eval* nil))
                             (declare (ignore))
                             nil))))
              (declare (ignore name))
              ;; hand parse: token after "(defun "
              (let* ((r (string-left-trim " " (subseq line 6))))
                (unless (and (plusp (length r)) (char= (char r 0) #\())
                  (let ((end (position-if (lambda (c)
                                            (member c '(#\Space #\Tab #\( #\))))
                                          r)))
                    (when (and end (plusp end))
                      (push (string-upcase (subseq r 0 end)) names))))))))))
    (nreverse (remove-duplicates names :test #'string= :from-end t))))

;;; ---- world symbol index ----------------------------------------------------

(defstruct symidx by-name pkg-cache aliases)

(defun build-symbol-index (model)
  "pname -> list of symbol-block VMAs, scanning wired+unwired data pages once."
  (let ((by-name (make-hash-table :test #'equal)))
    (dolist (e (append (world-model-wired-map model)
                       (world-model-unwired-map model)))
      (when (= (map-entry-opcode e) +op-data-pages+)
        (let ((qv (map-entry-payload e))
              (base (map-entry-address e)))
          (dotimes (i (map-entry-count e))
            (multiple-value-bind (tag data) (qref qv i)
              (when (= tag +type-header-p+)
                (multiple-value-bind (htag hdata) (world-q model data)
                  (when (and htag
                             (member (tag-type htag)
                                     (list +type-header-i+ +type-header-p+))
                             (= (ldb (byte 2 30) hdata)
                                +array-element-type-character+)
                             (not (logbitp 23 hdata)))
                    (let ((pn (ignore-errors (w-string model data))))
                      (when pn (push (+ base i) (gethash pn by-name))))))))))))
    (multiple-value-bind (homes aliases) (world-symbol-homes model)
      (declare (ignore homes))
      (make-symidx :by-name by-name
                   :pkg-cache (make-hash-table)
                   :aliases aliases))))

(defun sym-home (model vma cache)
  "Home package primary name of symbol block at VMA, or NIL."
  (multiple-value-bind (ptag pdata) (world-q model (+ vma 4))
    (when (and ptag (= (tag-type ptag) +type-array+))
      (w-package-primary-name model pdata cache))))

(defun sym-fcell-band (model vma)
  "(values BAND DATA TAG) of the function cell (vma+2)."
  (multiple-value-bind (ft fd) (world-q model (+ vma 2))
    (values (and fd (ldb (byte 12 20) fd)) fd ft)))

(defun classify-band (band data)
  (cond ((null band) :unresolved)
        ((= band #x882) :cold)
        ((or (= band #x822) (= band #x823)) :qld)
        ((and data (>= data #xF0000000)) :wired)
        (t :other)))

;;; primary-name resolution for a file package attribute
(defun resolve-pkg (idx name)
  (or (gethash name (symidx-aliases idx)) name))

;;; ---- per-file audit --------------------------------------------------------

(defun audit-file (model idx sysdir spec)
  (let* ((path (sys-spec->path sysdir spec))
         (exists (probe-file path))
         (pkgattr (and exists (file-package-name path)))
         (filepkg (and pkgattr (resolve-pkg idx pkgattr)))
         (names (and exists (extract-defun-names path)))
         (cache (symidx-pkg-cache idx))
         (buckets (make-hash-table))         ; verdict-keyword -> count
         (band-hist (make-hash-table))       ; band -> count (chosen)
         (unres nil)
         (cold-names nil))
    (dolist (nm names)
      (let* ((cands (gethash nm (symidx-by-name idx)))
             ;; annotate each candidate with home + band
             (ann (mapcar (lambda (v)
                            (multiple-value-bind (band data tag)
                                (sym-fcell-band model v)
                              (list v (sym-home model v cache) band data tag)))
                          cands))
             ;; pick: prefer file-pkg home, else LISP/SI/GLOBAL fallbacks, else all
             (matched (and filepkg
                           (remove-if-not
                            (lambda (a) (equal (second a) filepkg)) ann)))
             (fallback (unless matched
                         (remove-if-not
                          (lambda (a)
                            (member (second a)
                                    '("COMMON-LISP" "LISP" "SYSTEM-INTERNALS"
                                      "GLOBAL" "SYMBOLICS-COMMON-LISP"
                                      "FUTURE-COMMON-LISP")
                                    :test #'equal))
                          ann)))
             (chosen (or matched fallback ann)))
        (if (null chosen)
            (progn (push nm unres) (incf (gethash :unresolved buckets 0)))
            ;; a name is COLD if ANY chosen candidate is cold band
            (let* ((verds (mapcar (lambda (a) (classify-band (third a) (fourth a)))
                                  chosen))
                   (v (cond ((member :cold verds) :cold)
                            ((member :qld verds) :qld)
                            ((member :wired verds) :wired)
                            ((member :other verds) :other)
                            (t :unresolved))))
              (incf (gethash v buckets 0))
              (dolist (a chosen)
                (when (third a) (incf (gethash (third a) band-hist 0))))
              (when (eq v :cold) (push nm cold-names))))))
    (let* ((ncold (gethash :cold buckets 0))
           (nqld (gethash :qld buckets 0))
           (nwired (gethash :wired buckets 0))
           (nother (gethash :other buckets 0))
           (nunres (gethash :unresolved buckets 0))
           (ndefun (length names))
           (verdict
             (cond ((not exists) :missing-source)
                   ((zerop ndefun) :no-defuns)
                   ((plusp ncold) :cold)
                   ((>= nqld 2) :qld-band)
                   ((= nqld 1) :qld-1)          ; weak
                   ((plusp nwired) :wired)
                   (t :indeterminate))))
      (list :spec spec :path (and exists (namestring path))
            :pkg pkgattr :ndefun ndefun
            :cold ncold :qld nqld :wired nwired :other nother :unres nunres
            :verdict verdict
            :cold-names (nreverse cold-names)
            :unres-names (nreverse unres)
            :band-hist (let (l) (maphash (lambda (k v) (push (cons k v) l)) band-hist)
                         (sort l #'> :key #'cdr))))))

;;; ---- driver ----------------------------------------------------------------

(defun read-filelist (path)
  (with-open-file (s path)
    (loop for line = (read-line s nil nil)
          while line
          for tl = (string-trim '(#\Space #\Tab #\Return) line)
          unless (or (zerop (length tl)) (char= (char tl 0) #\;))
            collect tl)))

(let* ((dist (second sb-ext:*posix-argv*))
       (sysdir (pathname (concatenate 'string (third sb-ext:*posix-argv*) "/")))
       (flist (fourth sb-ext:*posix-argv*))
       (model (read-world dist))
       (idx (build-symbol-index model))
       (specs (read-filelist flist)))
  (format t "~&~72,,,'-<~>~%")
  (format t "~&~34A ~4A ~4A ~4A ~4A ~4A  VERDICT~%"
          "FILE" "def" "cold" "qld" "wir" "unr")
  (format t "~&~72,,,'-<~>~%")
  (dolist (spec specs)
    (let ((r (audit-file model idx sysdir spec)))
      (format t "~&~34A ~4D ~4D ~4D ~4D ~4D  ~A~%"
              (subseq spec 0 (min 34 (length spec)))
              (getf r :ndefun) (getf r :cold) (getf r :qld)
              (getf r :wired) (getf r :unres) (getf r :verdict))))
  (format t "~&~72,,,'-<~>~%~%DETAIL:~%")
  (dolist (spec specs)
    (let ((r (audit-file model idx sysdir spec)))
      (format t "~&~A  [~A] pkg=~A defuns=~D  verdict=~A~%"
              spec (or (getf r :path) "NO-SOURCE") (getf r :pkg)
              (getf r :ndefun) (getf r :verdict))
      (format t "~&    bands: ~{~3,'0X:~D~^ ~}~%"
              (loop for (b . c) in (getf r :band-hist) append (list b c)))
      (when (getf r :cold-names)
        (format t "~&    COLD-band fns (~D): ~{~A~^ ~}~%"
                (length (getf r :cold-names))
                (subseq (getf r :cold-names) 0 (min 12 (length (getf r :cold-names))))))
      (when (getf r :unres-names)
        (format t "~&    unresolved (~D): ~{~A~^ ~}~%"
                (length (getf r :unres-names))
                (subseq (getf r :unres-names) 0 (min 12 (length (getf r :unres-names)))))))))
