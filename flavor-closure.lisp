;;; Flavor component-closure analyzer (read-only, raw-text parse).
;;; usage: sbcl --script flavor-closure.lisp SYSDIR FILELIST.txt [ROOT ...]
;;; Parses (defflavor NAME (ivars) (components) ...) and
;;; (compile-flavor-methods ...) forms from every file, builds
;;; flavor->components, then reports for each CFM root the transitive
;;; component closure and any component NOT defflavored anywhere in the set.

(defun spec->path (sysdir spec)
  (let* ((body (string-trim " " (subseq spec (1+ (position #\: spec)))))
         (parts (loop with s = 0
                      for semi = (position #\; body :start s)
                      collect (string-trim " " (subseq body s (or semi (length body))))
                      while semi do (setf s (1+ semi)))))
    (format nil "~A~{~A/~}~A.lisp" sysdir
            (mapcar #'string-downcase (butlast parts))
            (string-downcase (car (last parts))))))

(defun slurp (path)
  (with-open-file (s path :if-does-not-exist nil :external-format :latin-1)
    (when s
      (let ((str (make-string (file-length s))))
        (subseq str 0 (read-sequence str s))))))

(defun strip-pkg (tok)
  "Drop package prefix and trailing junk; upcase."
  (let* ((c (position #\: tok :from-end t))
         (base (if c (subseq tok (1+ c)) tok)))
    (string-upcase (string-trim "()'`," base))))

(defun read-balanced (str pos)
  "STR[pos]=#\\( ; return (values inner-string end-pos) spanning the group."
  (let ((depth 0) (i pos) (n (length str)) (in-str nil) (in-cmt nil))
    (loop while (< i n) do
      (let ((c (char str i)))
        (cond (in-cmt (when (char= c #\Newline) (setf in-cmt nil)))
              (in-str (cond ((char= c #\\) (incf i))
                            ((char= c #\") (setf in-str nil))))
              ((char= c #\;) (setf in-cmt t))
              ((char= c #\") (setf in-str t))
              ((char= c #\() (incf depth))
              ((char= c #\)) (decf depth)
               (when (zerop depth)
                 (return-from read-balanced (values (subseq str (1+ pos) i) (1+ i))))))
        (incf i)))
    (values (subseq str (1+ pos) i) i)))

(defun tokens (str)
  "Whitespace/paren-split tokens (drops parens), keeping symbol chars."
  (let ((toks nil) (cur (make-string-output-stream)) (in-cmt nil) (in-str nil))
    (flet ((flush () (let ((s (get-output-stream-string cur)))
                       (when (plusp (length s)) (push s toks)))))
      (loop for c across str do
        (cond (in-cmt (when (char= c #\Newline) (setf in-cmt nil)))
              (in-str (when (char= c #\") (setf in-str nil)))
              ((char= c #\;) (setf in-cmt t) (flush))
              ((char= c #\") (setf in-str t) (flush))
              ((member c '(#\( #\) #\Space #\Tab #\Newline #\Return))
               (flush))
              (t (write-char c cur))))
      (flush))
    (nreverse toks)))

(defun find-form (str head start)
  "Find next case-insensitive \"(HEAD\" boundary at/after START.
Returns the position of the #\\( or NIL."
  (let ((n (length str)) (hl (length head)) (i start))
    (loop while (< i n) do
      (let ((p (search head str :start2 i :test #'char-equal)))
        (unless p (return nil))
        ;; require '(' just before, and a delimiter just after
        (when (and (> p 0) (char= (char str (1- p)) #\()
                   (or (>= (+ p hl) n)
                       (member (char str (+ p hl))
                               '(#\Space #\Tab #\Newline #\Return #\())))
          (return (1- p)))
        (setf i (1+ p))))))

(defun parse-file (path flavors cfms)
  "Populate FLAVORS (name->components) and CFMS (list of (file . flavor-list))."
  (let ((str (slurp path)))
    (when str
      ;; defflavors
      (let ((i 0))
        (loop for op = (find-form str "defflavor" i)
              while op do
          (multiple-value-bind (inner end) (read-balanced str op)
            (setf i end)
            ;; inner = "defflavor NAME (ivars) (comps) ..."
            ;; token 1 = NAME ; then first paren group = ivars, second = comps
            (let* ((after-head (subseq inner (+ (search "defflavor" inner :test #'char-equal) 9)))
                   (nm-end (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\()))
                                        (string-left-trim '(#\Space #\Tab #\Newline #\Return) after-head)))
                   (nm-start (- (length after-head)
                                (length (string-left-trim '(#\Space #\Tab #\Newline #\Return) after-head))))
                   (name (strip-pkg (subseq after-head nm-start (and nm-end (+ nm-start nm-end)))))
                   ;; find the two paren groups after the name
                   (p1 (position #\( after-head :start (or (+ nm-start (or nm-end 0)) 0))))
              (when p1
                (multiple-value-bind (ig e1) (read-balanced after-head p1)
                  (declare (ignore ig))
                  (let ((p2 (position #\( after-head :start e1)))
                    (when p2
                      (multiple-value-bind (comps e2) (read-balanced after-head p2)
                        (declare (ignore e2))
                        (setf (gethash name flavors)
                              (mapcar #'strip-pkg (tokens comps))))))))))))
      ;; compile-flavor-methods
      (let ((i 0))
        (loop for op = (find-form str "compile-flavor-methods" i)
              while op do
          (multiple-value-bind (inner end) (read-balanced str op)
            (setf i end)
            (let ((toks (tokens inner)))
              ;; drop the head token, keep flavor names (skip &optional etc)
              (push (cons (pathname-name path)
                          (mapcar #'strip-pkg (remove-if
                                               (lambda (t2) (char-equal (char t2 0) #\&))
                                               (rest toks))))
                    (cdr (last cfms))))))))))

(let* ((sysdir (concatenate 'string (second sb-ext:*posix-argv*) "/"))
       (flist (third sb-ext:*posix-argv*))
       (roots (cdddr sb-ext:*posix-argv*))
       (flavors (make-hash-table :test #'equal))
       (cfms (list :head))
       (specs (with-open-file (s flist)
                (loop for line = (read-line s nil nil) while line
                      for tl = (string-trim '(#\Space #\Tab #\Return) line)
                      unless (or (zerop (length tl)) (char= (char tl 0) #\;))
                        collect tl))))
  (dolist (spec specs)
    (let ((p (spec->path sysdir spec)))
      (when (probe-file p) (parse-file p flavors cfms))))
  (setf cfms (rest cfms))
  (format t "~&Parsed ~D defflavors, ~D CFM forms from ~D files.~%"
          (hash-table-count flavors) (length cfms) (length specs))
  ;; closure check
  (labels ((closure (name seen)
             (unless (gethash name seen)
               (setf (gethash name seen) t)
               (let ((comps (gethash name flavors :missing)))
                 (unless (eq comps :missing)
                   (dolist (c comps) (closure c seen))))))
           (report-root (label flavs)
             (let ((seen (make-hash-table :test #'equal)))
               (dolist (f flavs) (closure f seen))
               (let ((missing (loop for f being the hash-keys of seen
                                    unless (nth-value 1 (gethash f flavors))
                                      collect f)))
                 (format t "~&~%### ~A  roots=~{~A~^ ~}~%" label flavs)
                 (format t "    closure size ~D~%" (hash-table-count seen))
                 (if missing
                     (format t "    !! components NOT defflavored in set (~D): ~{~A~^ ~}~%"
                             (length missing) (sort missing #'string<))
                     (format t "    OK: every component defflavored in set.~%"))))))
    (dolist (cfm cfms)
      (report-root (format nil "CFM in ~A" (car cfm)) (cdr cfm)))
    (when roots
      (report-root "explicit roots" (mapcar #'strip-pkg roots)))))
