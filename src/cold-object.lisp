;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: materializing decoded .vbin objects as Ivory memory.
;;;
;;; Layouts mirror i-sys/sysdef.lisp (DEFSTORAGE ARRAY :528, number formats
;;; :380-430, *ARRAY-TYPE-CODES* :560) and the allocator's leader discipline
;;; (sys/icons.lisp:1725: [leader-header][elements reversed][array-header]
;;; [data], leader element i read at header-1-i, stub/ifunarra.c:3186).
;;;
;;; The entry point is COLD-REF: decoded object -> (values tag data) of the
;;; Q that references it, materializing blocks on first sight.  Host conses
;;; carry identity (vbin table slots alias list tails), so cell VMAs are
;;; interned in an EQ table and lists materialize in two phases (allocate
;;; and register cells, then fill cars) to support self-reference.

(in-package #:worldtool)

(defvar *cold-default-package* "SYSTEM-INTERNALS"
  "Package name for vsyms with :DEFAULT package, bound per input file from
its attribute list.")

;;; Provisional area regions.  Bases are placeholders in the unwired oblast
;;; space until M3e derives the real address-space policy; sizes are ample
;;; for the 85-file cold set.  Ground truth puts the heap at #x80000000+.
(defparameter *cold-heap-regions*
  '(("SYMBOL-AREA"            #x80100000 #x100000)
    ("PNAME-AREA"             #x80300000 #x100000)
    ("PROPERTY-LIST-AREA"     #x80500000 #x100000)
    ("PERMANENT-STORAGE-AREA" #x80700000 #x400000)
    ("WORKING-STORAGE-AREA"   #x80C00000 #x100000)
    ("COMPILED-FUNCTION-AREA" #x81000000 #x800000)
    ("DEBUG-INFO-AREA"        #x81900000 #x400000)
    ("CONSTANTS-AREA"         #x81E00000 #x100000)))

(defun cold-add-heap-regions (w)
  (loop for (name origin length) in *cold-heap-regions*
        do (cold-add-region w name origin length)))

;;; EQ identity for materialized host objects (conses, varrays, vfuns).
(defvar *cold-object-vmas*)

(defmacro with-cold-materializer ((w) &body body)
  (declare (ignore w))
  `(let ((*cold-object-vmas* (make-hash-table :test #'eq)))
     ,@body))

;;; ---------------- Strings ----------------

;;; *ARRAY-TYPE-CODES* enumeration order (i-sys/sysdef.lisp:560); the code
;;; is the array header's 6-bit type field: element-type(2) packing(3)
;;; list-bit(1).  Verified against ground truth: ART-STRING #o24 gives the
;;; #x50000006 header of a 6-char pname; ART-Q #o60 gives the #xC0000400
;;; region-table headers.
(defparameter *cold-array-type-codes*
  '(("ART-FIXNUM" . #o00) ("ART-16B" . #o02) ("ART-8B" . #o04)
    ("ART-4B" . #o06) ("ART-2B" . #o10) ("ART-1B" . #o12)
    ("ART-FAT-STRING" . #o20) ("ART-STRING" . #o24)
    ("ART-BOOLEAN" . #o52)
    ("ART-Q" . #o60) ("ART-Q-LIST" . #o61)))

(defun cold-array-type-code (w name)
  (declare (ignore w))
  (or (cdr (assoc name *cold-array-type-codes* :test #'string=))
      (error "~A not an array type" name)))

(defun cold-string* (w string area)
  "Materialize STRING as a fresh ART-STRING block; returns the header VMA."
  (let* ((len (length string))
         (nwords (ceiling len 4))
         (vma (cold-alloc w area (1+ nwords)))
         (art-string (cold-array-type-code w "ART-STRING")))
    (cw-set w vma (tag 1 (cold-dtp w "HEADER-I"))
            (logior (ash art-string 26) len))
    (dotimes (i nwords)
      (let ((word 0))
        (loop for b from 0 below 4
              for ci = (+ (* i 4) b)
              while (< ci len)
              do (setf word (logior word
                                    (ash (logand (char-code (char string ci))
                                                 #xFF)
                                         (* 8 b)))))
        (cw-set w (+ vma 1 i) (tag 0 (cold-dtp w "FIXNUM")) word)))
    vma))

(defun cold-pname (w string)
  "Interned copy of STRING in PNAME-AREA (symbol pnames, package names)."
  (or (gethash string (cold-world-strings w))
      (setf (gethash string (cold-world-strings w))
            (cold-string* w string "PNAME-AREA"))))

;;; ---------------- Symbols ----------------

(defun canonical-package-name (package)
  "Fold a vsym/vpackage package spec to the name string stored in the cold
symbol's package cell (PKG-FIND-PACKAGE resolves it at first boot)."
  (etypecase package
    ((eql :default) *cold-default-package*)
    ((eql :uninterned) nil)
    (string package)
    (vpackage (canonical-package-name (vpackage-spec package)))
    (cons (error "Package spec ~S not yet supported" package))))

(defun cold-symbol (w pname package-name)
  "Intern (PNAME, PACKAGE-NAME); returns the symbol block VMA."
  (let ((key (cons pname package-name)))
    (or (and package-name (gethash key (cold-world-symbols w)))
        (let* ((vma (cold-alloc w "SYMBOL-AREA" 5))
               (dtp-null (cold-dtp w "NULL")))
          (when package-name
            (setf (gethash key (cold-world-symbols w)) vma))
          (cw-set w vma (tag 0 (cold-dtp w "HEADER-P")) (cold-pname w pname))
          (cw-set w (+ vma 1) (tag 0 dtp-null) vma)    ; unbound value
          (cw-set w (+ vma 2) (tag 0 dtp-null) vma)    ; unbound function
          (multiple-value-bind (ntag ndata) (cold-nil-q w)
            (cw-set w (+ vma 3) ntag ndata))           ; empty plist
          (if package-name
              (cw-set w (+ vma 4) (tag 0 (cold-dtp w "STRING"))
                      (cold-pname w package-name))
              (multiple-value-bind (ntag ndata) (cold-nil-q w)
                (cw-set w (+ vma 4) ntag ndata)))
          vma))))

(defun cold-vsym (w vsym)
  "Symbol VMA for a decoded vsym.  NIL and T fold to the architectural
blocks; uninterned symbols keep identity through the vsym object itself."
  (let ((name (vsym-name vsym))
        (package (canonical-package-name (vsym-package vsym))))
    (cond ((and (string= name "NIL") package) (cold-world-nil-vma w))
          ((and (string= name "T") package) (cold-world-t-vma w))
          ((null package)
           (or (gethash vsym *cold-object-vmas*)
               (setf (gethash vsym *cold-object-vmas*)
                     (cold-symbol w name nil))))
          (t (cold-symbol w name package)))))

(defun cold-symbol-ref (w vsym)
  (let ((vma (cold-vsym w vsym)))
    (if (= vma (cold-world-nil-vma w))
        (cold-nil-q w)
        (values (tag 0 (cold-dtp w "SYMBOL")) vma))))

;;; ---------------- Lists ----------------

(defun cold-list (w cell area)
  "Materialize the cons CELL (and the cells reachable through its cdr chain)
with cdr-coding; returns CELL's VMA.  Cells already materialized -- shared
tails -- terminate the run with a cdr-normal link."
  (or (gethash cell *cold-object-vmas*)
      (let ((cells nil) (tail nil))
        ;; The maximal not-yet-materialized run starting at CELL.
        (loop for c = cell then (cdr c)
              do (cond ((null c) (return))
                       ((not (consp c)) (setf tail c) (return))
                       ((gethash c *cold-object-vmas*)
                        (setf tail c) (return))
                       (t (push c cells))))
        (setf cells (nreverse cells))
        (let* ((n (length cells))
               (last-shared (or (consp tail) (and tail t)))
               (nqs (if last-shared (1+ n) n))
               (vma (cold-alloc w area nqs)))
          ;; Phase 1: register cell VMAs (self-references resolve).
          (loop for c in cells
                for i from 0
                do (setf (gethash c *cold-object-vmas*) (+ vma i)))
          ;; Phase 2: fill cars, then the closing Q.
          (loop for c in cells
                for i from 0
                for lastp = (= i (1- n))
                do (multiple-value-bind (cart card) (cold-ref w (car c) :area area)
                     (cw-set w (+ vma i)
                             (logior (ash (cond ((and lastp last-shared)
                                                 +cdr-normal+)
                                                (lastp +cdr-nil+)
                                                (t +cdr-next+))
                                          6)
                                     (tag-type cart))
                             card)))
          (when last-shared
            (multiple-value-bind (ttag tdata) (cold-ref w tail :area area)
              (cw-set w (+ vma n) ttag tdata)))
          (gethash cell *cold-object-vmas*)))))

;;; ---------------- Numbers ----------------

(defun cold-bignum (w n area)
  (let* ((mag (abs n))
         (len (ceiling (integer-length mag) 32))
         (vma (cold-alloc w area (1+ len))))
    (cw-set w vma (tag (layout-value (cold-world-layout w)
                                     "SYSTEM:%HEADER-TYPE-NUMBER")
                       (cold-dtp w "HEADER-I"))
            (logior (ash (layout-value (cold-world-layout w)
                                       "SYSTEM:%HEADER-SUBTYPE-BIGNUM")
                         28)
                    (if (minusp n) (ash 1 27) 0)
                    len))
    (dotimes (i len)
      (cw-set w (+ vma 1 i) (tag 0 (cold-dtp w "FIXNUM"))
              (ldb (byte 32 (* 32 i)) mag)))
    vma))

(defun cold-double (w bits area)
  "DTP-DOUBLE-FLOAT points at a bare 2-Q cell, stored big-endian
(sysdef.lisp DEFSTORAGE DOUBLE-FLOAT)."
  (let ((vma (cold-alloc w area 2))
        (fixnum (cold-dtp w "FIXNUM")))
    (cw-set w vma (tag +cdr-next+ fixnum) (ldb (byte 32 32) bits))
    (cw-set w (1+ vma) (tag +cdr-nil+ fixnum) (ldb (byte 32 0) bits))
    vma))

(defun oldfloat-single-bits (v)
  "IEEE single bits for an obsolete BIN-OP-FLOAT (mantissa, exponent) pair."
  (let ((f (float (* (if (voldfloat-negative v) -1 1)
                     (voldfloat-mantissa v)
                     (expt 2 (voldfloat-exponent v)))
                  1.0f0)))
    (single-bits f)))

(defun single-bits (f)
  (let ((bits (sb-kernel:single-float-bits (float f 1.0f0))))
    (ldb (byte 32 0) bits)))

;;; ---------------- Arrays ----------------

(defun varray-option (options name)
  "Look up :NAME in the flat (keyword value ...) option list."
  (loop for (opt val) on options by #'cddr
        when (and (vsym-p opt) (string= (vsym-name opt) name))
          return (values val t)))

(defun cold-array-type (w varray)
  "(values type-code element-type packing) from the array's options."
  (multiple-value-bind (type typep) (varray-option (varray-options varray) "TYPE")
    (let* ((name (cond ((not typep) "ART-Q")
                       ((vsym-p type) (vsym-name type))
                       (t (error "Array :TYPE ~S unsupported" type))))
           (code (cold-array-type-code w name)))
      (values code (ldb (byte 2 4) code) (ldb (byte 3 1) code)))))

(defun cold-array (w varray area)
  "Materialize a decoded array.  Short-prefix one-dimensional arrays only;
multi-dimensional and displaced arrays error until the cold set demands
them.  Returns the array HEADER vma."
  (or (gethash varray *cold-object-vmas*)
      (let ((dims (varray-dimensions varray)))
        (unless (and (= (length dims) 1) (integerp (first dims)))
          (error "Multi-dimensional array ~S not yet supported" dims))
        (multiple-value-bind (type-code element-type packing)
            (cold-array-type w varray)
          (declare (ignore element-type))
          (let* ((len (first dims))
                 (per-word (ash 1 packing))
                 (options (varray-options varray))
                 (leader-length
                   (or (varray-option options "LEADER-LENGTH") 0))
                 (leader-list (varray-option options "LEADER-LIST"))
                 (fill-pointer (varray-option options "FILL-POINTER"))
                 (nwords (if (zerop packing) len (ceiling len per-word)))
                 (named-structure
                   (varray-option options "NAMED-STRUCTURE-SYMBOL")))
            (when (>= len (ash 1 15))
              (error "Array of ~D elements needs a long prefix" len))
            (when (and fill-pointer (zerop leader-length))
              (setf leader-length 1))
            (let* ((total (+ (if (zerop leader-length) 0 (1+ leader-length))
                             1 nwords))
                   (base (cold-alloc w area total))
                   (header (if (zerop leader-length)
                               base
                               (+ base 1 leader-length))))
              (setf (gethash varray *cold-object-vmas*) header)
              ;; Leader: header-p/leader Q pointing at the array header,
              ;; then elements reversed (element i at header-1-i).
              (unless (zerop leader-length)
                (cw-set w base
                        (tag (layout-value (cold-world-layout w)
                                           "SYSTEM:%HEADER-TYPE-LEADER")
                             (cold-dtp w "HEADER-P"))
                        header)
                (dotimes (i leader-length)
                  (let ((value (cond ((and (zerop i) fill-pointer) fill-pointer)
                                     ((nth i leader-list) (nth i leader-list))
                                     (t nil))))
                    (multiple-value-bind (vt vd) (cold-ref w value :area area)
                      (cw-set w (- header 1 i) vt vd)))))
              (cw-set w header (tag 1 (cold-dtp w "HEADER-I"))
                      (logior (ash type-code 26)
                              (if named-structure (ash 1 25) 0)
                              (ash leader-length 15)
                              len))
              (when named-structure
                ;; Named-structure symbol lives in leader 1 (or element 0?)
                ;; -- resolved when struct-cold materialization lands (M3e).
                nil)
              ;; Data
              (cond
                ((varray-words varray)
                 (let ((words (varray-words varray)))
                   (dotimes (i nwords)
                     (let ((low (if (< (* 2 i) (length words))
                                    (aref words (* 2 i)) 0))
                           (high (if (< (1+ (* 2 i)) (length words))
                                     (aref words (1+ (* 2 i))) 0)))
                       (cw-set w (+ header 1 i)
                               (tag 0 (cold-dtp w "FIXNUM"))
                               (logior low (ash high 16)))))))
                ((varray-contents varray)
                 (unless (zerop packing)
                   (error "Boxed contents in a packed array"))
                 (let ((contents (varray-contents varray)))
                   (dotimes (i len)
                     (multiple-value-bind (vt vd)
                         (cold-ref w (aref contents i) :area area)
                       (cw-set w (+ header 1 i) vt vd)))))
                (t
                 ;; Uninitialized: NIL for object arrays, 0 for packed.
                 (if (zerop packing)
                     (multiple-value-bind (ntag ndata) (cold-nil-q w)
                       (dotimes (i len)
                         (cw-set w (+ header 1 i) ntag ndata)))
                     (dotimes (i nwords)
                       (cw-set w (+ header 1 i)
                               (tag 0 (cold-dtp w "FIXNUM")) 0)))))
              header))))))

;;; ---------------- The reference dispatcher ----------------

(defun cold-ref (w obj &key (area "PERMANENT-STORAGE-AREA"))
  "The Q referencing OBJ, materializing storage on first sight.
Returns (values tag data)."
  (etypecase obj
    (null (cold-nil-q w))
    ((eql t) (values (tag 0 (cold-dtp w "SYMBOL")) (cold-world-t-vma w)))
    (integer
     (if (typep obj '(signed-byte 32))
         (values (tag 0 (cold-dtp w "FIXNUM")) (ldb (byte 32 0) obj))
         (values (tag 0 (cold-dtp w "BIGNUM")) (cold-bignum w obj area))))
    (ratio
     (let ((num (numerator obj)) (den (denominator obj)))
       (if (and (typep num '(signed-byte 16)) (typep den '(unsigned-byte 16)))
           (values (tag 0 (cold-dtp w "SMALL-RATIO"))
                   (logior (ash (ldb (byte 16 0) num) 16) den))
           (error "Big ratio ~S not yet supported" obj))))
    (string
     (values (tag 0 (cold-dtp w "STRING")) (cold-string* w obj area)))
    (cons
     (values (tag 0 (cold-dtp w "LIST")) (cold-list w obj area)))
    (vsym (cold-symbol-ref w obj))
    (vloc
     (let ((target (vloc-target obj)))
       (unless (vsym-p target)
         (error "Locative to ~S not yet supported" target))
       (values (tag 0 (cold-dtp w "LOCATIVE"))
               (+ (cold-vsym w target)
                  (ecase (vloc-kind obj) (:value 1) (:function 2))))))
    (vchar
     (when (or (vchar-charset obj) (vchar-style obj))
       (error "Styled character ~S not yet supported" obj))
     (values (tag 0 (cold-dtp w "CHARACTER"))
             (logior (ash (or (vchar-bits obj) 0) 28) (vchar-code obj))))
    (vsingle
     (values (tag 0 (cold-dtp w "SINGLE-FLOAT")) (vsingle-bits obj)))
    (voldfloat
     (values (tag 0 (cold-dtp w "SINGLE-FLOAT")) (oldfloat-single-bits obj)))
    (vdouble
     (values (tag 0 (cold-dtp w "DOUBLE-FLOAT"))
             (cold-double w (vdouble-bits obj) area)))
    (varray
     (let ((header (cold-array w obj area)))
       (multiple-value-bind (code et packing) (cold-array-type w obj)
         (declare (ignore code packing))
         (values (tag 0 (cold-dtp w (if (= et 1) "STRING" "ARRAY")))
                 header))))
    (character  ; strings decode to CL chars only via veval forms
     (values (tag 0 (cold-dtp w "CHARACTER")) (char-code obj)))))
