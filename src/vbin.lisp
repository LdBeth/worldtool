;;; -*- Mode: Lisp -*-
;;; .vbin reader: Genera BIN file format, version 5, IMach/VLM variant.
;;;
;;; Semantics mirrored from the Genera sources SYS:L-BIN;DEFS.LISP (opcodes),
;;; SYS:L-BIN;LOAD.LISP (loader), SYS:L-BIN;DUMP.LISP (dumper), cross-checked
;;; against SYS:L-BIN;UNBIN.LISP.  The stream is a sequence of 16-bit words,
;;; little-endian on disk.  A command word is (high 4 bits . low 12 bits);
;;; high = #xF escapes the low 12 bits into the command dispatch, otherwise
;;; high is the command and low is an immediate operand.
;;;
;;; Table discipline (the subtle part): every FOR-VALUE command reserves the
;;; NEXT table index when the command STARTS (before its sub-values are read)
;;; and stores its value there on completion.  This matches the dumper, which
;;; calls ENTER-TABLE before dumping components.  INITIALIZE-LIST reserves one
;;; slot per cons cell (the successive tails), before reading the elements, so
;;; shared/circular list structure works.
;;;
;;; Genera objects that have no host equivalent decode into the V* structs
;;; below; lists, strings, and integers decode into native CL objects.

(in-package #:worldtool)

;;; ---- decoded-object model ----------------------------------------------

(defstruct (vsym (:constructor make-vsym (package name)))
  ;; PACKAGE is :default (BIN-OP-SYMBOL, interned in the file's package),
  ;; a string, a refname list (strings, possibly ending in vsym INTERNAL),
  ;; a cons ("syntax" . "name"), a vpackage, or :uninterned.
  package name)

(defstruct (vpackage (:constructor make-vpackage (spec)))
  spec)                                 ; string or ("ZL" . "USER")

(defstruct (vloc (:constructor make-vloc (kind target)))
  kind                                  ; :value or :function
  target)                               ; symbol / function spec

(defstruct (vchar (:constructor make-vchar (code bits &optional charset style)))
  code bits charset style)

(defstruct (vstyle (:constructor make-vstyle (family face size attributes)))
  family face size attributes)

(defstruct (vcharset (:constructor make-vcharset (name)))
  name)

(defstruct (vsingle (:constructor make-vsingle (bits)))
  bits)                                 ; raw IEEE-754 single bits

(defstruct (vdouble (:constructor make-vdouble (bits)))
  bits)                                 ; raw IEEE-754 double bits

(defstruct (voldfloat (:constructor make-voldfloat (negative mantissa exponent)))
  negative mantissa exponent)           ; obsolete BIN-OP-FLOAT encoding

(defstruct (vcomplex (:constructor make-vcomplex (realpart imagpart)))
  realpart imagpart)

(defstruct (varray (:constructor make-varray (dimensions options)))
  dimensions options
  contents                              ; vector of Qs (boxed arrays)
  words                                 ; vector of raw 16-bit data (numeric)
  floatp)                               ; convert-array-to-floating seen

(defstruct (vinstance (:constructor make-vinstance (flavor plist)))
  flavor plist)

(defstruct (veval (:constructor make-veval (form)))
  form)                                 ; BIN-OP-FORM: evaluate at load time

(defstruct (vembed (:constructor make-vembed (dtp-code offset)))
  dtp-code                              ; 0=list 1=lex-closure 2=dyn-closure
                                        ; 3=double 4=big-ratio 5=complex
                                        ; 6=compiled-function 7=locative
  offset)                               ; word offset from CCA base

(defstruct (vnative (:constructor make-vnative (word)))
  word)                                 ; 32 bits, tag dtp-spare-immediate-1

;; One Q of a compiled function body.  OP is the raw 16-bit opcode-tag:
;; bits 0-7 tag byte, bit 8 type-from-tag, bit 9 immediate, bit 10 relative.
;; DATA is a 32-bit unsigned integer (immediate) or a decoded object.
(defstruct (vword (:constructor make-vword (op data)))
  op data)

(defstruct (vfun (:constructor make-vfun (total-size suffix-size name-and-storage words)))
  total-size suffix-size
  name-and-storage                      ; (fspec [(si:storage-category ...)] [(compiler:area ...)])
  words)                                ; vector of VWORD, length total-size - 2

(defstruct (vop (:constructor make-vop (op args)))
  op                                    ; :fdefine :setq :putprop :defconst
                                        ; :defvar :initialize-array
                                        ; :initialize-numeric-array
                                        ; :attribute-list
  args)

;; Whole-file result.
(defstruct vbin-file
  path version attributes events table-size words-read words-total padding)

;;; ---- printing ------------------------------------------------------------

(defun vsym-print-name (v)
  (let ((pkg (vsym-package v))
        (name (vsym-name v)))
    (etypecase pkg
      ((eql :default) name)
      ((eql :uninterned) (format nil "#:~A" name))
      (string (format nil "~A:~A" pkg name))
      (vpackage (format nil "~A:~A" (vpackage-spec pkg) name))
      (cons (if (stringp (cdr pkg))     ; ("ZL" . "USER") syntax pair
                (format nil "~A|~A:~A" (car pkg) (cdr pkg) name)
                (format nil "~{~A~^:~}:~A"
                        (mapcar (lambda (x) (if (vsym-p x) (vsym-name x) x)) pkg)
                        name))))))

(defmethod print-object ((v vsym) stream)
  (write-string (vsym-print-name v) stream))

(defmethod print-object ((v vpackage) stream)
  (format stream "#<pkg ~A>" (vpackage-spec v)))

(defmethod print-object ((v vloc) stream)
  (format stream "#<~A-cell ~A>"
          (ecase (vloc-kind v) (:value "value") (:function "function"))
          (vloc-target v)))

(defmethod print-object ((v vchar) stream)
  (format stream "#\\{~D~@[.~D~]~@[ ~A~]}"
          (vchar-code v)
          (let ((b (vchar-bits v))) (and (not (eql b 0)) b))
          (vchar-charset v)))

(defmethod print-object ((v vsingle) stream)
  (format stream "~F" (decode-ieee-single (vsingle-bits v))))

(defmethod print-object ((v vdouble) stream)
  (format stream "~F" (decode-ieee-double (vdouble-bits v))))

(defmethod print-object ((v veval) stream)
  (format stream "(**EVAL** ~S)" (veval-form v)))

(defmethod print-object ((v varray) stream)
  (format stream "#<array ~S~@[ ~S~]~@[ ~D-elt~]~@[ ~D-word~]>"
          (varray-dimensions v) (varray-options v)
          (and (varray-contents v) (length (varray-contents v)))
          (and (varray-words v) (length (varray-words v)))))

(defmethod print-object ((v vfun) stream)
  (format stream "#<function ~A ~D+~DQ>"
          (let ((ns (vfun-name-and-storage v)))
            (if (consp ns) (first ns) ns))
          (- (vfun-total-size v) (vfun-suffix-size v))
          (vfun-suffix-size v)))

(defmethod print-object ((v vop) stream)
  (format stream "#<~A~{ ~S~}>" (vop-op v) (vop-args v)))

(defun decode-ieee-single (bits)
  (let ((sign (if (logbitp 31 bits) -1 1))
        (expt (ldb (byte 8 23) bits))
        (frac (ldb (byte 23 0) bits)))
    (cond ((= expt 255)
           (cond ((not (zerop frac)) :nan)
                 ((minusp sign) :-infinity)
                 (t :infinity)))
          ((zerop expt) (* sign (scale-float (float frac 1d0) -149)))
          (t (* sign (scale-float (float (+ frac (ash 1 23)) 1d0) (- expt 150)))))))

(defun decode-ieee-double (bits)
  (let ((sign (if (logbitp 63 bits) -1 1))
        (expt (ldb (byte 11 52) bits))
        (frac (ldb (byte 52 0) bits)))
    (cond ((= expt 2047)
           (cond ((not (zerop frac)) :nan)
                 ((minusp sign) :-infinity)
                 (t :infinity)))
          ((zerop expt) (* sign (scale-float (float frac 1d0) -1074)))
          (t (* sign (scale-float (float (+ frac (ash 1 52)) 1d0) (- expt 1075)))))))

;;; ---- word stream -----------------------------------------------------------

(defstruct (vstream (:constructor make-vstream (bytes)))
  bytes (pos 0))

(defun vstream-nwords (s) (floor (length (vstream-bytes s)) 2))

(defun vnext-word (s)
  (let ((bytes (vstream-bytes s))
        (i (* 2 (vstream-pos s))))
    (when (> (+ i 2) (length bytes))
      (error "unexpected end of .vbin data at word ~D" (vstream-pos s)))
    (incf (vstream-pos s))
    (logior (aref bytes i) (ash (aref bytes (1+ i)) 8))))

;;; ---- load table -------------------------------------------------------------

(defvar *vtable*)

(defconstant +vunset+ '+vunset+)

(defun vreserve-slot ()
  (vector-push-extend +vunset+ *vtable*)
  (1- (fill-pointer *vtable*)))

(defun vstore-slot (index value)
  (setf (aref *vtable* index) value)
  value)

(defun vfetch-slot (index)
  (let ((v (aref *vtable* index)))
    (when (eq v +vunset+)
      (error "fetch of unset bin table slot ~D" index))
    v))

;;; ---- command dispatch -------------------------------------------------------

(defconstant +no-value+ '+no-value+)

(defparameter *vbin-op-names*
  '((0 . number-immediate) (1 . table-fetch-immediate) (2 . string-immediate)
    (3 . list-immediate) (4 . list*-immediate) (5 . array)
    (6 . defconst) (7 . defvar) (8 . make-instance-immediate)
    (9 . embedded-constant-immediate) (10 . initialize-list-immediate)
    (#o20 . form) (#o21 . eof) (#o22 . file-attribute-list)
    (#o23 . format-version) (#o24 . table-fetch) (#o25 . table-store)
    (#o26 . symbol) (#o27 . package-symbol) (#o30 . string) (#o31 . list)
    (#o32 . list*) (#o33 . integer) (#o34 . negative-integer)
    (#o35 . ratio) (#o36 . float) (#o37 . negative-float)
    (#o40 . value-cell-location) (#o41 . fdefinition-location)
    (#o42 . fdefine) (#o43 . setq) (#o44 . putprop)
    (#o45 . l-compiled-function) (#o46 . initialize-array)
    (#o47 . initialize-numeric-array) (#o50 . table-fetch-medium)
    (#o51 . initialize-and-return-array)
    (#o52 . initialize-and-return-numeric-array)
    (#o53 . small-character) (#o54 . character) (#o55 . extended-number)
    (#o56 . convert-array-to-floating) (#o57 . table-fetch-large)
    (#o60 . character-style) (#o61 . character-set) (#o62 . 32-bit-fixnum)
    (#o63 . ieee-single-float) (#o64 . ieee-double-float) (#o65 . complex)
    (#o66 . i-compiled-function) (#o67 . package)
    (#o70 . table-store-initializer) (#o71 . initialize-list)
    (#o72 . native-instruction)))

(defun vbin-op-name (index)
  (or (cdr (assoc index *vbin-op-names*)) index))

;; Decode one command; returns (values value op-index).
(defun vbin-next-command (s)
  (let* ((word (vnext-word s))
         (high (ldb (byte 4 12) word))
         (low (ldb (byte 12 0) word))
         (index (if (= high #xF) low high))
         (arg (if (= high #xF) nil low)))
    (values (vbin-run-command s index arg) index)))

;; Decode one command and require it to produce a value.
(defun vbin-value (s)
  (let ((v (vbin-next-command s)))
    (when (eq v +no-value+)
      (error "expected a value, got a for-effect bin command"))
    v))

(defun vfalse-p (x)
  (or (null x) (and (vsym-p x) (string= (vsym-name x) "NIL"))))

(defun vbin-run-command (s index arg)
  (macrolet ((for-value (&body body)
               ;; Reserve the table slot BEFORE reading sub-values.
               `(let ((.slot. (vreserve-slot)))
                  (vstore-slot .slot. (progn ,@body)))))
    (ecase index
      ;; --- immediate-operand ops ---
      (0 (if (logbitp 11 arg) (- arg 4096) arg))        ; number-immediate
      (1 (vfetch-slot arg))                             ; table-fetch-immediate
      (2 (for-value (read-vstring s arg)))              ; string-immediate
      (3 (for-value (read-vlist s arg)))                ; list-immediate
      (4 (for-value (read-vlist* s arg)))               ; list*-immediate
      (5 (for-value                                     ; array
          (let ((dims (vbin-value s)))
            (make-varray dims (loop repeat (* 2 arg) collect (vbin-value s))))))
      (6 (make-vop :defconst (read-vvalues s arg)))     ; defconst
      (7 (make-vop :defvar (read-vvalues s arg)))       ; defvar
      (8 (let ((flavor (vbin-value s)))                 ; make-instance-immediate
           (make-vinstance flavor (read-vvalues s arg))))
      (9 (make-vembed (ldb (byte 3 0) arg)              ; embedded-constant-immediate
                      (ldb (byte 9 3) arg)))
      (10 (read-new-vlist s (logbitp 11 arg)            ; initialize-list-immediate
                          (ldb (byte 11 0) arg)))
      ;; --- escaped command ops ---
      (#o20 (make-veval (vbin-value s)))                ; form
      (#o21 (throw :vbin-eof t))                        ; eof
      (#o22 (make-vop :attribute-list                   ; file-attribute-list
                      (list (vbin-value s))))
      (#o23 (let ((version (vbin-value s)))             ; format-version
              (unless (member version '(4 5))
                (error "unsupported BIN format version ~S" version))
              version))
      (#o24 (vfetch-slot (vbin-value s)))               ; table-fetch
      (#o25 (for-value (vbin-value s)))                 ; table-store
      (#o26 (for-value (make-vsym :default (vbin-value s)))) ; symbol
      (#o27 (for-value                                  ; package-symbol
             (let* ((pkg (vbin-value s))
                    (name (vbin-value s)))
               (make-vsym (if (vfalse-p pkg) :uninterned pkg) name))))
      (#o30 (for-value (read-vstring s (vbin-value s)))) ; string
      (#o31 (for-value (read-vlist s (vbin-value s))))   ; list
      (#o32 (for-value (read-vlist* s (vbin-value s))))  ; list*
      (#o33 (for-value (read-vinteger s (vbin-value s)))) ; integer
      (#o34 (for-value (- (read-vinteger s (vbin-value s))))) ; negative-integer
      (#o35 (for-value                                  ; ratio
             (let* ((num (vbin-value s)) (den (vbin-value s)))
               (/ num den))))
      (#o36 (for-value                                  ; float (obsolete)
             (let* ((m (vbin-value s)) (e (vbin-value s)))
               (make-voldfloat nil m e))))
      (#o37 (for-value                                  ; negative-float
             (let* ((m (vbin-value s)) (e (vbin-value s)))
               (make-voldfloat t m e))))
      (#o40 (for-value (make-vloc :value (vbin-value s)))) ; value-cell-location
      (#o41 (for-value (make-vloc :function (vbin-value s)))) ; fdefinition-location
      (#o42 (make-vop :fdefine                          ; fdefine
                      (let* ((fspec (vbin-value s)) (def (vbin-value s)))
                        (list fspec def))))
      (#o43 (make-vop :setq                             ; setq
                      (let* ((sym (vbin-value s)) (val (vbin-value s)))
                        (list sym val))))
      (#o44 (make-vop :putprop (read-vvalues s 3)))     ; putprop
      (#o45 (error "BIN-OP-L-COMPILED-FUNCTION: 3600 function in a .vbin"))
      (#o46 (read-initialize-array s :boxed)            ; initialize-array
            +no-value+)
      (#o47 (read-initialize-array s :numeric)          ; initialize-numeric-array
            +no-value+)
      (#o50 (vfetch-slot (vnext-word s)))               ; table-fetch-medium
      (#o51 (read-initialize-array s :boxed))           ; initialize-and-return-array
      (#o52 (read-initialize-array s :numeric))         ; initialize-and-return-numeric-array
      (#o53 (for-value                                  ; small-character
             (let ((n (vbin-value s)))
               (make-vchar (ldb (byte 8 0) n) (ldb (byte 4 8) n)))))
      (#o54 (for-value                                  ; character
             (let* ((bits (vbin-value s))
                    (charset (vbin-value s))
                    (subindex (vbin-value s))
                    (style (vbin-value s)))
               (make-vchar subindex bits charset style))))
      (#o55 (for-value                                  ; extended-number (obsolete)
             (let ((length (vbin-value s))
                   (header-type (vbin-value s)))
               (unless (eql length 2)
                 (error "extended number of size ~S" length))
               (let* ((a (vbin-value s)) (b (vbin-value s)))
                 (ecase header-type
                   (1 (/ a b))
                   (2 (make-vcomplex a b))
                   (3 (make-vdouble (logior (ash a 32) b))))))))
      (#o56 (let ((array (vbin-value s)))               ; convert-array-to-floating
              (when (varray-p array) (setf (varray-floatp array) t))
              array))
      (#o57 (let* ((low (vnext-word s))                 ; table-fetch-large
                   (high (vnext-word s)))
              (vfetch-slot (logior low (ash high 16)))))
      (#o60 (for-value                                  ; character-style
             (let* ((family (vbin-value s)) (face (vbin-value s))
                    (size (vbin-value s)) (attrs (vbin-value s)))
               (make-vstyle family face size attrs))))
      (#o61 (for-value (make-vcharset (vbin-value s)))) ; character-set
      (#o62 (let* ((low (vnext-word s))                 ; 32-bit-fixnum (signed)
                   (high (vnext-word s))
                   (u (logior low (ash high 16))))
              (if (logbitp 31 u) (- u #x100000000) u)))
      (#o63 (let* ((high (vnext-word s))                ; ieee-single, MS word first
                   (low (vnext-word s)))
              (make-vsingle (logior (ash high 16) low))))
      (#o64 (let* ((w (loop repeat 4 collect (vnext-word s)))) ; ieee-double, MS first
              (make-vdouble (logior (ash (first w) 48) (ash (second w) 32)
                                    (ash (third w) 16) (fourth w)))))
      (#o65 (let* ((re (vbin-value s)) (im (vbin-value s))) ; complex
              (make-vcomplex re im)))
      (#o66 (for-value (read-vfun s)))                  ; i-compiled-function
      (#o67 (for-value (make-vpackage (vbin-value s)))) ; package
      (#o70 (prog1 (for-value (vbin-value s))           ; table-store-initializer
              (vbin-next-command s)))
      (#o71 (let* ((dotify (vbin-value s))              ; initialize-list
                   (count (vbin-value s)))
              (read-new-vlist s (not (vfalse-p dotify)) count)))
      (#o72 (let* ((low (vnext-word s))                 ; native-instruction
                   (high (vnext-word s)))
              (make-vnative (logior low (ash high 16))))))))

;;; ---- readers for composite objects ----------------------------------------

(defun read-vvalues (s n)
  (loop repeat n collect (vbin-value s)))

(defun read-vstring (s length)
  (let ((string (make-string length)))
    (loop with i = 0
          while (< i length)
          do (let ((word (vnext-word s)))
               (setf (char string i) (code-char (ldb (byte 8 0) word)))
               (incf i)
               (when (< i length)
                 (setf (char string i) (code-char (ldb (byte 8 8) word)))
                 (incf i))))
    string))

(defun read-vlist (s length)
  (read-vvalues s length))

(defun read-vlist* (s length)
  ;; N values; the last one is the final cdr: (v1 ... v_{n-1} . v_n)
  (let ((values (read-vvalues s length)))
    (if (= length 1)
        (first values)
        (apply #'list* values))))

;; INITIALIZE-LIST: reserve one table slot per cons cell (the tails) before
;; reading the elements, so elements can reference the list being built.
(defun read-new-vlist (s dotify count)
  (if (zerop count)
      (if dotify (vbin-value s) nil)
      (let ((list (make-list count)))
        (loop for l on list
              do (vstore-slot (vreserve-slot) l))
        (loop for l on list
              do (setf (car l) (vbin-value s)))
        (when dotify
          (setf (cdr (last list)) (vbin-value s)))
        list)))

(defun read-vinteger (s length)
  ;; LENGTH 16-bit words, least significant first, unsigned magnitude.
  (loop with n = 0
        for i from 0 below length
        do (setf n (logior n (ash (vnext-word s) (* 16 i))))
        finally (return n)))

;; INITIALIZE-ARRAY / INITIALIZE-NUMERIC-ARRAY (and the -AND-RETURN- forms):
;; array value, length value, then LENGTH boxed values or raw 16-bit words.
(defun read-initialize-array (s kind)
  (let* ((array (vbin-value s))
         (length (vbin-value s)))
    (ecase kind
      (:boxed
       (let ((contents (make-array length)))
         (dotimes (i length)
           (setf (aref contents i) (vbin-value s)))
         (when (varray-p array)
           (setf (varray-contents array) contents))))
      (:numeric
       (let ((words (make-array length :element-type '(unsigned-byte 16))))
         (dotimes (i length)
           (setf (aref words i) (vnext-word s)))
         (when (varray-p array)
           (setf (varray-words array) words)))))
    array))

;; BIN-OP-I-COMPILED-FUNCTION: total-size, suffix-size, name-and-storage as
;; values, then (total-size - 2) Qs, each a 16-bit opcode-tag optionally
;; followed by a 32-bit immediate (two words, least significant first) or a
;; nested value.  2 = DEFSTORAGE-SIZE of COMPILED-FUNCTION on Ivory.
(defun read-vfun (s)
  (let* ((total-size (vbin-value s))
         (suffix-size (vbin-value s))
         (name-and-storage (vbin-value s))
         (n (- total-size 2))
         (words (make-array n)))
    (dotimes (i n)
      (let* ((op (vnext-word s))
             (data (if (logbitp 9 op)   ; %%I-COMPILED-FUNCTION-IMMEDIATE
                       (let* ((low (vnext-word s))
                              (high (vnext-word s)))
                         (logior low (ash high 16)))
                       (vbin-value s))))
        (setf (aref words i) (make-vword op data))))
    (make-vfun total-size suffix-size name-and-storage words)))

;;; ---- SYS: logical host --------------------------------------------------

;; Genera pathnames like "SYS: IO; RDDEFS" are CL logical pathnames once the
;; blanks after the delimiters go; aim the SYS host at the restored source
;; tree and translate-logical-pathname does the rest (including the
;; customary-case downcasing to Unix names).  SBCL predefines a SYS host for
;; its own sources, so track our takeover with a flag rather than probing.
(defvar *sys-host-root* nil)

(defun setup-sys-host (&optional (root "/Users/ldbeth/Public/symbolics/rel-8-5/sys/"))
  (setf (logical-pathname-translations "SYS")
        `(("SYS:**;*.*" ,(merge-pathnames "**/*.*" root))))
  (setf *sys-host-root* root))

(defun sys-pathname (spec &optional (type "vbin"))
  "Translate a Genera pathname string like \"SYS: IO; RDDEFS\" to a host pathname."
  (unless *sys-host-root*
    (setup-sys-host))
  (translate-logical-pathname
   (format nil "~A.~A" (remove #\Space spec) (string-upcase type))))

;;; ---- whole-file entry -------------------------------------------------------

(defun read-vbin (path)
  (let* ((bytes (with-open-file (f path :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length f)
                                       :element-type '(unsigned-byte 8))))
                    (read-sequence v f)
                    v)))
         (s (make-vstream bytes))
         (*vtable* (make-array 1024 :adjustable t :fill-pointer 0))
         (version nil)
         (attributes nil)
         (events '()))
    (catch :vbin-eof
      (loop
        (multiple-value-bind (value op) (vbin-next-command s)
          (case (vbin-op-name op)
            (format-version (setf version value))
            (file-attribute-list (setf attributes (first (vop-args value))))
            (eof)
            (t (push (cons (vbin-op-name op) value) events))))))
    (let ((read (vstream-pos s))
          (total (vstream-nwords s)))
      ;; LMFS->host copies pad the tail with zeros, but recompiles written
      ;; in place over a longer file leave words of the OLD file's last
      ;; event + EOF behind.  The EOF command was explicitly parsed, so a
      ;; nonzero tail is stale bytes, not truncation -- warn, don't die.
      (loop for i from read below total
            for word = (vnext-word s)
            unless (zerop word)
              do (warn "~A: ~D nonzero words after EOF (first: #x~4,'0X ~
at word ~D) -- stale tail from an in-place overwrite"
                       path (- total i) word i)
                 (loop-finish))
      (make-vbin-file :path path :version version :attributes attributes
                      :events (nreverse events)
                      :table-size (fill-pointer *vtable*)
                      :words-read read :words-total total
                      :padding (- total read)))))

;;; ---- CLI ---------------------------------------------------------------------

(defun vbin-attribute (vbin name)
  (loop for (key value) on (vbin-file-attributes vbin) by #'cddr
        when (and (vsym-p key) (string-equal (vsym-name key) name))
          return value))

(defun summarize-vbin (vbin &key trace)
  (let ((*print-circle* t))
    (format t "~&~A: BIN version ~D, ~D words (~D padding), ~D table slots~%"
            (vbin-file-path vbin) (vbin-file-version vbin)
            (vbin-file-words-total vbin) (vbin-file-padding vbin)
            (vbin-file-table-size vbin))
    (format t "  package ~A, base ~A, syntax ~A~%"
            (vbin-attribute vbin "PACKAGE")
            (vbin-attribute vbin "BASE")
            (vbin-attribute vbin "SYNTAX"))
    (let ((counts '()))
      (loop for (op . value) in (vbin-file-events vbin)
            for key = (if (and (vop-p value) (eq op 'form)) :form
                          (if (vop-p value) (vop-op value) op))
            do (let ((entry (assoc key counts)))
                 (if entry (incf (cdr entry)) (push (cons key 1) counts))))
      (format t "  top level:~{ ~D ~A~^,~}~%"
              (loop for (key . n) in (nreverse counts) nconc (list n key))))
    (when trace
      (loop for (op . value) in (vbin-file-events vbin)
            do (format t "~&  ~A: ~S~%" op value)))))

(defun vbin-world (paths &key trace)
  "Decode each of PATHS, printing a summary (or full trace); return NIL on any failure."
  (let ((failures 0))
    (dolist (path paths)
      (handler-case (summarize-vbin (read-vbin path) :trace trace)
        (error (e)
          (incf failures)
          (format t "~&~A: FAILED: ~A~%" path e))))
    (when (> (length paths) 1)
      (format t "~&~D file~:P, ~D failure~:P~%" (length paths) failures))
    (zerop failures)))
