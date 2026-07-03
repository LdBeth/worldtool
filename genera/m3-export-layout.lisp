;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; M3 prep: export the layout tables the cold-load generator needs, dumped
;;; from the running OG2 VLM Genera world (Load File, then (cl-user::m3-export-layout)).
;;;
;;; Rather than reimplementing the SYSDEF expanders (i-compiler/i-sysdef-support.lisp)
;;; on the SBCL side, we dump their runtime traces:
;;;   DEFSYSCONSTANT  -> :SYSTEM-CONSTANT property + constant value
;;;   DEFSYSBYTE      -> SYS:DEFSYSBYTE property = (n-bits bits-over)
;;;   DEFSTORAGE      -> SYS:DEFSTORAGE-SIZE property on the structure name;
;;;                      per-field accessor macros carry a FUNCTION-PARENT
;;;                      declaration (parent-type SYS:DEFSTORAGE) and expand to
;;;                      (SI:COMPACT-DEFSTORAGE-ACCESSOR obj packed-word [full-offset])
;;;   DEFINE-MAGIC-LOCATIONS -> *MAGIC-LOCATIONS* alist (name start end . ventries)
;;;   cold-load areas -> AREA-LIST, each name's value is its area number
;;;
;;; Internal symbols whose home package we cannot be sure of (the %%DEFSTORAGE-*
;;; bytes' defining file is not in the restored source tree) are resolved with
;;; FIND-SYMBOL at run time, so a wrong package guess shows up as a clear error
;;; or a "missing" report instead of a silently empty section.
;;;
;;; Output is a .sexp file of pure data (keywords, strings, integers, lists)
;;; readable by stock SBCL READ.  Symbols are encoded as "PKG:NAME" strings;
;;; non-integer values as (:SYM "PKG:NAME"), (:STR "...") or (:OBJ "printed").

(defparameter *m3-layout-output* "SYS: SITE; COLD-LAYOUT.SEXP"
  "Where the layout dump is written.  Copy to the host tree afterwards.")

;;; Enumeration list variables (DEFENUMERATED and friends) worth dumping with
;;; their groupings, from grepping the kernel sources for DEFENUMERATED plus
;;; the cold-load tables in sys/ldata.lisp.  Missing ones are reported.
(defparameter *m3-enumerations*
  '("*DATA-TYPES*" "*ARRAY-TYPES*"
    "*EMB-CHANNEL-TYPES*" "*EMB-CONSOLE-COMMANDS*" "*EMB-CONSOLE-INITIAL-STATES*"
    "*EMB-DISK-REQUEST-OPCODES*" "*EMB-DISK-STATUS-CODES*" "*EMB-FEP-STATUS-CODES*"
    "*EMB-HOST-FILE-CLOSE-MODES*" "*EMB-HOST-FILE-COMMAND-OPCODES*"
    "*EMB-HOST-FILE-ERROR-CODES*" "*EMB-HOST-FILE-OPEN-DIRECTIONS*"
    "*EMB-MESSAGE-CHANNEL-SUBTYPES*" "*EMB-RESET-REQUEST-TYPES*"
    "*EMB-SCSI-COMMAND-OPCODES*"
    "*COPROCESSOR-ATTACH-DISK-CHANNEL-IF-NOT-FOUND-ACTION*"
    "*INTERRUPT-MODES*" "*ECC-ERROR-STATUS*"))

;;; Cold-load support tables (arrays or scalars) from sys/ldata.lisp.
(defparameter *m3-tables*
  '("*ARRAY-BITS-PER-ELEMENT*" "*ARRAY-ELEMENTS-PER-Q*" "*ARRAY-NULL-ELEMENT*"
    "*ARRAY-NULL-WORD*" "*VALID-ARRAY-TYPE-CODES*" "*DATA-TYPE-NAME*"
    "*INTERNAL-READABLE-REGISTER-MAP*" "*INTERNAL-WRITABLE-REGISTER-MAP*"))

(defvar *m3-missing* nil "Names from the static lists not found in this world.")

(defun m3-resolve (name)
  "Find the symbol named NAME in the system packages (inherited included)."
  (or (find-symbol name "SI") (find-symbol name "SYS") (find-symbol name "GLOBAL")))

(defun m3-lookup (name)
  (let ((sym (m3-resolve name)))
    (unless sym (pushnew name *m3-missing* :test #'equal))
    sym))

(defun m3-name (sym)
  (let ((pkg (symbol-package sym)))
    (format nil "~A:~A" (if pkg (package-name pkg) "#UNINTERNED") (symbol-name sym))))

(defun m3-value (val)
  (cond ((integerp val) val)
	((symbolp val) (list :sym (m3-name val)))
	((stringp val) (list :str (string val)))
	(t (list :obj (scl:condition-case ()
			   (prin1-to-string val)
			 (error "#<unprintable>"))))))

(defun m3-unsigned (pointer)
  (ldb (byte 32. 0) (sys:%pointer pointer)))

(defun m3-signed-ldb (bytespec word)
  (let ((raw (ldb bytespec word))
	(half (ash 1 (1- (byte-size bytespec)))))
    (- (logxor raw half) half)))

;;; One pass over every symbol of every package, deduplicated.
(defun m3-map-all-symbols (fn)
  (let ((seen (make-hash-table :test #'eq :size 400000.)))
    (dolist (pkg (list-all-packages))
      (zl:mapatoms #'(lambda (sym)
		    (unless (gethash sym seen)
		      (setf (gethash sym seen) t)
		      (funcall fn sym)))
		pkg nil))))

;;; Resolved once per export: the accessor symbol, FUNCTION-PARENT, and the
;;; %%DEFSTORAGE-* byte specs (plist of keyword -> byte spec).
(defvar *m3-cdsa*)
(defvar *m3-function-parent*)
(defvar *m3-ds-bytes*)

(defparameter *m3-ds-byte-names*
  '((:structure "%%DEFSTORAGE-STRUCTURE")
    (:forwardable "%%DEFSTORAGE-FORWARDABLE")
    (:preserve-cdr-codes "%%DEFSTORAGE-PRESERVE-CDR-CODES")
    (:fixnum-only "%%DEFSTORAGE-FIXNUM-ONLY")
    (:check-fixnum-only "%%DEFSTORAGE-CHECK-FIXNUM-ONLY")
    (:physical "%%DEFSTORAGE-PHYSICAL")
    (:offset "%%DEFSTORAGE-OFFSET")
    (:position "%%DEFSTORAGE-POSITION")
    (:size "%%DEFSTORAGE-SIZE")))

(defun m3-setup-defstorage-decode ()
  (setq *m3-cdsa* (or (m3-resolve "COMPACT-DEFSTORAGE-ACCESSOR")
		      (error "COMPACT-DEFSTORAGE-ACCESSOR not found")))
  (setq *m3-function-parent* (or (m3-resolve "FUNCTION-PARENT")
				 (error "FUNCTION-PARENT not found")))
  (setq *m3-ds-bytes*
	(loop for (key name) in *m3-ds-byte-names*
	      as sym = (or (m3-resolve name) (error "~A not found" name))
	      unless (boundp sym) do (error "~A is unbound" name)
	      append (list key (symbol-value sym)))))

(defun m3-ds-byte (key) (getf *m3-ds-bytes* key))

(defun m3-defstorage-field-record (sym)
  "If SYM is a DEFSTORAGE field-accessor macro, return its decoded record."
  (multiple-value-bind (parent parent-type)
      (scl:condition-case () (funcall *m3-function-parent* sym) (error nil))
    (when (and parent (eq parent-type 'sys:defstorage))
      (let ((exp (scl:condition-case ()
		     (macroexpand-1 (list sym 'm3-dummy-object))
		   (error nil))))
	(when (and (consp exp)
		   (eq (first exp) *m3-cdsa*)
		   (integerp (third exp)))
	  (let* ((word (third exp))
		 (full (fourth exp))
		 (offset (if (integerp full)
			     full
			     (m3-signed-ldb (m3-ds-byte :offset) word)))
		 (flags (loop for key in '(:structure :forwardable :preserve-cdr-codes
					   :fixnum-only :check-fixnum-only :physical)
			      when (ldb-test (m3-ds-byte key) word)
				collect key)))
	    (list (m3-name parent) (m3-name sym)
		  word (if (integerp full) full nil)
		  offset
		  (ldb (m3-ds-byte :position) word)
		  (ldb (m3-ds-byte :size) word)
		  flags)))))))

(defun m3-export-layout ()
  (setq *m3-missing* nil)
  (m3-setup-defstorage-decode)
  (let ((constants nil) (sysbytes nil) (sizes nil) (fields nil))
    ;; Single sweep for the property-marked definitions.
    (m3-map-all-symbols
      #'(lambda (sym)
	  (when (and (get sym ':system-constant) (boundp sym))
	    (push (list (m3-name sym) (m3-value (symbol-value sym))) constants))
	  (let ((byte (get sym 'sys:defsysbyte)))
	    (when byte
	      (push (list (m3-name sym) (first byte) (second byte)) sysbytes)))
	  (let ((size (get sym 'sys:defstorage-size)))
	    (when size
	      (push (list (m3-name sym) size) sizes)))
	  (when (macro-function sym)
	    (let ((record (m3-defstorage-field-record sym)))
	      (when record (push record fields))))))
    (setq fields (sort fields #'(lambda (a b)
				  (if (string-equal (first a) (first b))
				      (< (fifth a) (fifth b))
				      (string-lessp (first a) (first b))))))
    (with-open-file (s *m3-layout-output* :direction :output)
      (let ((*print-base* 10.) (*print-radix* nil) (*print-pretty* nil)
	    (*print-length* nil) (*print-level* nil))
	(format s ";;; Cold-load layout tables dumped from a running Genera world.~%")
	(format s ";;; Generated by worldtool/genera/m3-export-layout.lisp.~%")
	(flet ((section (title records)
		 (format s "~%(~S" title)
		 (dolist (r records) (format s "~% ~S" r))
		 (format s "~% )~%")))
	  (section :system-constants (sort constants #'string-lessp :key #'first))
	  (section :defsysbytes (sort sysbytes #'string-lessp :key #'first))
	  ;; DTP-* and friends predate the :SYSTEM-CONSTANT convention, so also
	  ;; sweep everything integer-valued interned in the SYSTEM package.
	  (let ((sys-integers nil))
	    (zl:mapatoms #'(lambda (sym)
			  (when (and (boundp sym) (integerp (symbol-value sym)))
			    (push (list (m3-name sym) (symbol-value sym)) sys-integers)))
		      (find-package "SYSTEM") nil)
	    (section :sys-integers (sort sys-integers #'string-lessp :key #'first)))
	  (section :defstorage-sizes (sort sizes #'string-lessp :key #'first))
	  (section :defstorage-fields fields)
	  (let ((enums nil))
	    (dolist (name *m3-enumerations*)
	      (let ((sym (m3-lookup name)))
		(when (and sym (boundp sym) (listp (symbol-value sym)))
		  (push (list (m3-name sym) (mapcar #'m3-value (symbol-value sym)))
			enums))))
	    (section :enumerations (nreverse enums)))
	  (let ((tables nil))
	    (dolist (name *m3-tables*)
	      (let ((sym (m3-lookup name)))
		(when (and sym (boundp sym))
		  (let ((val (symbol-value sym)))
		    (push (list (m3-name sym)
				(if (arrayp val)
				    (loop for i below (length val)
					  collect (m3-value (aref val i)))
				    (m3-value val)))
			  tables)))))
	    (section :tables (nreverse tables)))
	  (let ((ml (m3-lookup "*MAGIC-LOCATIONS*")))
	    (section :magic-locations
		     (when (and ml (boundp ml))
		       (loop for (name address end . ventries) in (symbol-value ml)
			     collect (list (m3-name name)
					   (m3-unsigned address)
					   (m3-unsigned end)
					   (loop for (type val) in ventries
						 collect (list type (m3-value val))))))))
	  (let ((al (m3-lookup "AREA-LIST")))
	    (section :areas
		     (when (and al (boundp al))
		       (loop for area in (symbol-value al)
			     collect (list (m3-name area)
					   (if (boundp area) (symbol-value area) nil)))))))))
    (format t "~&Wrote ~A:~@
		 ~5D system constants~@
		 ~5D defsysbytes~@
		 ~5D defstorage structures, ~D fields~%"
	    *m3-layout-output* (length constants) (length sysbytes)
	    (length sizes) (length fields))
    (when *m3-missing*
      (format t "~&Not found in this world (expected for some non-VLM configs):~%~{  ~A~%~}"
	      *m3-missing*))))

(format t "~&M3 layout exporter loaded.  Run: (cl-user::m3-export-layout)~@
	     then copy the output to MAC:/Users/ldbeth/Public/symbolics/rel-8-5/cold-layout.sexp~%")
