;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: the PKGDCL pass (M3f).
;;;
;;; SYS:SYS;PKGDCL is :lisp-read-only -- never compiled; the original cold
;;; load generator reads it "magically ... in cooperation with
;;; BUILD-INITIAL-PACKAGES" and only DEFPACKAGE forms are allowed
;;; (pkgdcl.lisp:58-60).  Despite the -*- Base: 10 -*- attribute line the
;;; file is read in base 8 (pkgdcl.lisp:76-77).  The generator expands
;;; each DEFPACKAGE into its DEFPACKAGE-INTERNAL call (the macro's
;;; expansion minus the RECORD-SOURCE-FILE-NAME guard; package.lisp:452)
;;; and stores the list of calls as the value of SI:BUILD-INITIAL-PACKAGES
;;; (package.lisp:2350-2356).  At first boot the function of the same name
;;; EVALs the list three times over -- create, shadow, import-export --
;;; with DEFPACKAGE-INTERNAL rebound per pass (package.lisp:2378-2425).

(in-package #:worldtool)

;;; ---- A Genera-enough source reader --------------------------------------
;;; Produces the same host structures the vbin decoder yields (vsyms, host
;;; lists, strings, integers) so COLD-REF materializes them identically.
;;; Coverage = what PKGDCL actually uses: lists, strings, |escaped| tokens,
;;; ; and #||...||# comments, #+/#- feature conditionals, base-8 integers
;;; with trailing-dot decimals (and the Zetalisp rule that a digit 8 or 9
;;; forces decimal), and # as a mid-token constituent
;;; (SET-SYNTAX-#-MACRO-CHAR is one symbol).

(defparameter *cold-source-features* '("IMACH" "VLM" "GENERA")
  "Feature set for #+/#- in cold source: the VLM is an IMach running
Genera.  3600 is deliberately absent.")

(defstruct (gsrc (:constructor make-gsrc (text)))
  text (pos 0))

(defconstant +gsrc-eof+ '+gsrc-eof+)
(defconstant +gsrc-close+ '+gsrc-close+)

(defun gsrc-peek (s)
  (let ((text (gsrc-text s)) (pos (gsrc-pos s)))
    (if (< pos (length text)) (char text pos) nil)))

(defun gsrc-next (s)
  (let ((ch (gsrc-peek s)))
    (when ch (incf (gsrc-pos s)))
    ch))

(defun gsrc-terminating-p (ch)
  (or (null ch)
      (member ch '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page
                   #\( #\) #\" #\;))))

(defun gsrc-skip (s)
  "Skip whitespace, ; line comments, and (nested) #|...|# block comments."
  (loop
    (let ((ch (gsrc-peek s)))
      (cond ((null ch) (return))
            ((member ch '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page))
             (gsrc-next s))
            ((char= ch #\;)
             (loop for c = (gsrc-next s)
                   until (or (null c) (char= c #\Newline))))
            ((and (char= ch #\#)
                  (< (1+ (gsrc-pos s)) (length (gsrc-text s)))
                  (char= (char (gsrc-text s) (1+ (gsrc-pos s))) #\|))
             (gsrc-next s) (gsrc-next s)
             (let ((depth 1))
               (loop while (plusp depth)
                     for c = (gsrc-next s)
                     do (cond ((null c) (error "EOF in #| comment"))
                              ((and (char= c #\#) (eql (gsrc-peek s) #\|))
                               (gsrc-next s) (incf depth))
                              ((and (char= c #\|) (eql (gsrc-peek s) #\#))
                               (gsrc-next s) (decf depth))))))
            (t (return))))))

(defun gsrc-read-string (s)
  (with-output-to-string (out)
    (loop for c = (gsrc-next s)
          do (cond ((null c) (error "EOF in string"))
                   ((char= c #\") (return))
                   ((char= c #\\) (write-char (or (gsrc-next s)
                                                  (error "EOF in string"))
                                              out))
                   (t (write-char c out))))))

(defun gsrc-read-token (s)
  "Accumulate one token; returns (values text escaped-p).  Unescaped
characters upcase; |...| segments preserve case.  # is a constituent
mid-token."
  (let ((out (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0))
        (escaped nil))
    (loop
      (let ((ch (gsrc-peek s)))
        (cond ((gsrc-terminating-p ch) (return))
              ((char= ch #\|)
               (setf escaped t)
               (gsrc-next s)
               (loop for c = (gsrc-next s)
                     do (cond ((null c) (error "EOF in |...|"))
                              ((char= c #\|) (return))
                              (t (vector-push-extend c out)))))
              ((char= ch #\\)
               (setf escaped t)
               (gsrc-next s)
               (vector-push-extend (or (gsrc-next s) (error "EOF after \\"))
                                   out))
              (t (vector-push-extend (char-upcase (gsrc-next s)) out)))))
    (values (coerce out 'string) escaped)))

(defvar *gsrc-feature-mode* nil
  "True while reading a #+/#- feature expression: tokens never classify
as numbers, so #+3600 sees the feature \"3600\", not the octal 1920.")

(defun gsrc-classify-token (text escaped)
  "Token -> integer or vsym.  Base 8, except a trailing dot or an 8/9
digit reads decimal (the Zetalisp rule pkgdcl relies on)."
  (unless (or escaped *gsrc-feature-mode*)
    (let* ((dot (and (> (length text) 1)
                     (char= (char text (1- (length text))) #\.)))
           (digits (if dot (subseq text 0 (1- (length text))) text)))
      (when (and (plusp (length digits))
                 (every #'digit-char-p digits))
        (return-from gsrc-classify-token
          (parse-integer digits
                         :radix (if (or dot (find #\8 digits)
                                        (find #\9 digits))
                                    10 8))))))
  ;; Symbol: KEYWORD / PKG:NAME / PKG::NAME / plain.
  (let ((colon (position #\: text)))
    (cond ((null colon) (make-vsym :default text))
          ((zerop colon) (make-vsym "KEYWORD" (subseq text 1)))
          (t (let* ((pkg (subseq text 0 colon))
                    (name-start (if (and (< (1+ colon) (length text))
                                         (char= (char text (1+ colon)) #\:))
                                    (+ colon 2)
                                    (1+ colon))))
               (make-vsym pkg (subseq text name-start)))))))

(defun gsrc-feature-true-p (expr)
  (etypecase expr
    (vsym (member (string-upcase (vsym-name expr)) *cold-source-features*
                  :test #'string=))
    (cons (let ((op (string-upcase (vsym-name (first expr)))))
            (cond ((string= op "OR")
                   (some #'gsrc-feature-true-p (rest expr)))
                  ((string= op "AND")
                   (every #'gsrc-feature-true-p (rest expr)))
                  ((string= op "NOT")
                   (not (gsrc-feature-true-p (second expr))))
                  (t (error "Feature operator ~A" op)))))))

(defun gsrc-read-form (s)
  "One form; +GSRC-EOF+ at end of input, +GSRC-CLOSE+ on a close paren."
  (loop
    (gsrc-skip s)
    (let ((ch (gsrc-peek s)))
      (cond
        ((null ch) (return +gsrc-eof+))
        ((char= ch #\() (gsrc-next s) (return (gsrc-read-list s)))
        ((char= ch #\)) (gsrc-next s) (return +gsrc-close+))
        ((char= ch #\") (gsrc-next s) (return (gsrc-read-string s)))
        ((char= ch #\')
         (gsrc-next s)
         (let ((form (gsrc-read-form s)))
           (when (symbolp form) (error "Quote before ~S" form))
           (return (list (make-vsym :default "QUOTE") form))))
        ((and (char= ch #\#)
              (< (1+ (gsrc-pos s)) (length (gsrc-text s)))
              (member (char (gsrc-text s) (1+ (gsrc-pos s))) '(#\+ #\-)))
         (gsrc-next s)
         (let* ((sense (char= (gsrc-next s) #\+))
                (feature (let ((*gsrc-feature-mode* t)) (gsrc-read-form s)))
                (form (gsrc-read-form s)))
           (when (or (symbolp feature) (symbolp form))
             (error "EOF inside #~:[-~;+~] conditional" sense))
           (when (eq (and (gsrc-feature-true-p feature) t) sense)
             (return form))))                     ; else loop: skip and re-read
        (t (return (multiple-value-call #'gsrc-classify-token
                     (gsrc-read-token s))))))))

(defun gsrc-read-list (s)
  (let ((items nil))
    (loop
      (let ((form (gsrc-read-form s)))
        (cond ((eq form +gsrc-close+) (return (nreverse items)))
              ((eq form +gsrc-eof+) (error "EOF inside a list"))
              (t (push form items)))))))

(defun read-genera-source (path)
  "All top-level forms of a Genera :LISP-READ-ONLY source file."
  (let ((s (make-gsrc (with-open-file (f path)
                        (let ((text (make-string (file-length f))))
                          (subseq text 0 (read-sequence text f)))))))
    (loop for form = (gsrc-read-form s)
          until (eq form +gsrc-eof+)
          when (eq form +gsrc-close+)
            do (error "Unbalanced close paren at ~D" (gsrc-pos s))
          collect form)))

;;; ---- DEFPACKAGE -> DEFPACKAGE-INTERNAL ----------------------------------

(defun pkgdcl-string (x)
  (etypecase x
    (string x)
    (vsym (vsym-name x))))

(defun pkgdcl-canonicalize (kw args)
  "Mirror package.lisp:455 CANONICALIZE; returns the (unquoted) value."
  (let ((name (vsym-name kw)))
    (flet ((single ()
             (unless (= (length args) 1)
               (error "More than one argument to the ~A keyword" name))
             (first args)))
      (cond ((member name '("SIZE" "EXTERNAL-ONLY" "NEW-SYMBOL-FUNCTION"
                            "HASH-INHERITED-SYMBOLS" "INVISIBLE" "COLON-MODE"
                            "PREFIX-INTERN-FUNCTION" "SYNTAX")
                     :test #'string=)
             (single))
            ((string= name "PREFIX-NAME") (pkgdcl-string (single)))
            ((member name '("NICKNAMES" "USE" "SHADOW" "EXPORT" "INCLUDE"
                            "SAFEGUARDED")
                     :test #'string=)
             (mapcar #'pkgdcl-string args))
            ((string= name "IMPORT-FROM")
             (let ((groups (if (consp (first args)) args (list args))))
               (mapcar (lambda (g) (mapcar #'pkgdcl-string g)) groups)))
            ;; :IMPORT / :SHADOWING-IMPORT / :RELATIVE-NAMES / etc: verbatim.
            (t args)))))

(defun cold-expand-defpackage (form)
  "(DEFPACKAGE name . clauses) -> host form
\(DEFPACKAGE-INTERNAL \"NAME\" :KW (QUOTE value) ...).
Second value: (pkg-name rel-name target-name) triples for any
:RELATIVE-NAMES clauses, which are withheld from the call.
MAKE-PACKAGE's :RELATIVE-NAMES path COPYTREEs the (name . package)
alist it builds (package.lisp:544-551); COPYLIST's (1+ (LENGTH list))
on the dotted pair ENDPs the package object, and pre-banner there are
no error tables to resume the trap -- ERROR-TRAP-HANDLER-1-COLD FERRORs
into the unbound-TERMINAL-IO recursion (M3h boot 14).  The names are
re-established by deferred SI:PKG-ADD-RELATIVE-NAME forms, the same
safe path RE-MAKE-PACKAGE-INTERNAL uses (package.lisp:1393,746).
\(:RELATIVE-NAMES-FOR-ME stays: MAKE-PACKAGE routes it through
PKG-ADD-RELATIVE-NAME already, and boots 10-13 executed those.)"
  (destructuring-bind (head name . clauses) form
    (unless (and (vsym-p head) (string= (vsym-name head) "DEFPACKAGE"))
      (error "PKGDCL contains a non-DEFPACKAGE form: ~S" head))
    (let* ((quote (make-vsym :default "QUOTE"))
           (pkg-name (pkgdcl-string name))
           (result (list pkg-name
                         (make-vsym :default "DEFPACKAGE-INTERNAL")))
           (relative nil))
      (dolist (clause clauses)
        (multiple-value-bind (kw args)
            (if (consp clause)
                (values (first clause) (rest clause))
                (values clause (list t)))
          (cond ((string= (vsym-name kw) "RELATIVE-NAMES")
                 ;; Mirror PROCESS-RELATIVE-NAME-SPEC (package.lisp:855):
                 ;; (name target...) fans out to one record per target.
                 (dolist (entry args)
                   (let ((rel (pkgdcl-string (first entry))))
                     (dolist (target (rest entry))
                       (push (list pkg-name rel (pkgdcl-string target))
                             relative)))))
                (t
                 (push kw result)
                 (push (list quote (pkgdcl-canonicalize kw args)) result)))))
      (values (nreverse result) (nreverse relative)))))

(defun cold-load-pkgdcl (w path)
  "Read PKGDCL, expand every DEFPACKAGE, and store the list of
DEFPACKAGE-INTERNAL calls as SI:BUILD-INITIAL-PACKAGES.  Withheld
:RELATIVE-NAMES triples land on (COLD-WORLD-RELATIVE-NAMES W) for
finalize to defer.  Returns the number of packages."
  (let* ((*cold-default-package* "SYSTEM-INTERNALS")
         (calls (mapcar (lambda (form)
                          (multiple-value-bind (call triples)
                              (cold-expand-defpackage form)
                            (dolist (triple triples)
                              (push triple (cold-world-relative-names w)))
                            call))
                        (read-genera-source path))))
    (multiple-value-bind (tag data)
        (cold-ref w calls :area "WORKING-STORAGE-AREA")
      (cold-set-symbol-value
       w (make-vsym "SYSTEM-INTERNALS" "BUILD-INITIAL-PACKAGES") tag data))
    (length calls)))

(defun cold-build-package-graph (path)
  "Fill *COLD-PACKAGE-USES* / *COLD-PACKAGE-IMPORTS* from PKGDCL for
COLD-RESOLVE-HOME's graph walk (cold-object.lisp), and fold pkgdcl
nicknames/prefix-names into *COLD-PACKAGE-ALIASES*.  Must run before any
vbin loads -- symbol interning depends on it.  A DEFPACKAGE without :USE
defaults to GLOBAL (package.lisp:503).  Returns the package count."
  (let ((uses (make-hash-table :test #'equal))
        (imports (make-hash-table :test #'equal))
        (count 0))
    (dolist (form (read-genera-source path))
      (destructuring-bind (head name . clauses) form
        (unless (and (vsym-p head) (string= (vsym-name head) "DEFPACKAGE"))
          (error "PKGDCL contains a non-DEFPACKAGE form: ~S" head))
        (incf count)
        (let ((pkg (pkgdcl-string name))
              (use-seen nil))
          (dolist (clause clauses)
            (multiple-value-bind (kw args)
                (if (consp clause)
                    (values (first clause) (rest clause))
                    (values clause nil))
              (let ((kwname (vsym-name kw)))
                (cond ((string= kwname "USE")
                       (setf use-seen t
                             (gethash pkg uses)
                             (mapcar #'pkgdcl-string args)))
                      ((string= kwname "IMPORT-FROM")
                       (let ((groups (if (consp (first args))
                                         args
                                         (list args))))
                         (dolist (g groups)
                           (let ((src (pkgdcl-string (first g))))
                             (dolist (s (rest g))
                               (setf (gethash (cons pkg (pkgdcl-string s))
                                              imports)
                                     src))))))
                      ((or (string= kwname "IMPORT")
                           (string= kwname "SHADOWING-IMPORT"))
                       ;; Explicitly qualified symbols; the vsym's own
                       ;; package is the source.
                       (dolist (s args)
                         (when (vsym-p s)
                           (let ((p (vsym-package s)))
                             (unless (member p '(:default :uninterned))
                               (setf (gethash (cons pkg (vsym-name s))
                                              imports)
                                     (canonical-package-name p)))))))
                      ((or (string= kwname "NICKNAMES")
                           (string= kwname "PREFIX-NAME"))
                       (when *cold-package-aliases*
                         (dolist (n args)
                           (let ((nn (pkgdcl-string n)))
                             (unless (gethash nn *cold-package-aliases*)
                               (setf (gethash nn *cold-package-aliases*)
                                     pkg))))))))))
          (unless use-seen
            (setf (gethash pkg uses) (list "GLOBAL"))))))
    (setf *cold-package-uses* uses
          *cold-package-imports* imports)
    count))
