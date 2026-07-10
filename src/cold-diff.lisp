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

;;; M3b: object materializers

(defun expect-q (w vma tag data what)
  (multiple-value-bind (atag adata) (cw-ref w vma)
    (cold-check (and (= atag tag) (= adata data))
                "~A at #x~8,'0X: expected ~2,'0X:~8,'0X got ~2,'0X:~8,'0X"
                what vma tag data atag adata)))

;;; (cold-read-string lives in cold-object.lisp with the other cold
;;; memory readers.)

(defun check-materializers (w)
  "M3b gate: each object kind materializes and reads back."
  (with-cold-checks ("cold materializers")
    (with-cold-materializer (w)
      (let ((fixnum (cold-dtp w "FIXNUM")))
        ;; Immediates
        (multiple-value-bind (tag data) (cold-ref w 42)
          (cold-check (and (= tag (tag 0 fixnum)) (= data 42)) "fixnum 42"))
        (multiple-value-bind (tag data) (cold-ref w -3)
          (cold-check (and (= tag (tag 0 fixnum)) (= data #xFFFFFFFD))
                      "fixnum -3"))
        (multiple-value-bind (tag data) (cold-ref w 1/2)
          (cold-check (and (= (tag-type tag) (cold-dtp w "SMALL-RATIO"))
                           (= data #x00010002))
                      "ratio 1/2"))
        ;; Bignum #x123456789A: 2 words LSW first
        (multiple-value-bind (tag data) (cold-ref w #x123456789A)
          (cold-check (= (tag-type tag) (cold-dtp w "BIGNUM")) "bignum tag")
          (multiple-value-bind (htag hdata) (cw-ref w data)
            (cold-check (and (= (tag-type htag) (cold-dtp w "HEADER-I"))
                             (= (ash htag -6) 2)  ; header-type number
                             (= hdata 2))         ; subtype 0, positive, len 2
                        "bignum header ~2,'0X:~8,'0X" htag hdata))
          (expect-q w (+ data 1) (tag 0 fixnum) #x3456789A "bignum low")
          (expect-q w (+ data 2) (tag 0 fixnum) #x12 "bignum high"))
        ;; String
        (multiple-value-bind (tag data) (cold-ref w "Benson")
          (cold-check (= (tag-type tag) (cold-dtp w "STRING")) "string tag")
          (multiple-value-bind (htag hdata) (cw-ref w data)
            (cold-check (and (= htag (tag 1 (cold-dtp w "HEADER-I")))
                             (= hdata #x50000006))
                        "string header: expected 43:50000006 got ~2,'0X:~8,'0X"
                        htag hdata))
          (expect-q w (+ data 1) (tag 0 fixnum) #x736E6542 "string data 0")
          (expect-q w (+ data 2) (tag 0 fixnum) #x00006E6F "string data 1")
          (cold-check (string= (cold-read-string w data) "Benson")
                      "string readback"))
        ;; Symbols: dedupe + NIL/T folding + package cell
        (let* ((foo (make-vsym "SYSTEM" "FOO"))
               (foo2 (make-vsym "SYSTEM" "FOO"))
               (v1 (cold-vsym w foo))
               (v2 (cold-vsym w foo2)))
          (cold-check (= v1 v2) "symbol dedupe")
          (multiple-value-bind (ptag pdata) (cw-ref w (+ v1 4))
            (declare (ignore pdata))
            (cold-check (= (tag-type ptag) (cold-dtp w "STRING"))
                        "package cell holds a string"))
          (cold-check (= (cold-vsym w (make-vsym "LISP" "NIL"))
                         (cold-world-nil-vma w))
                      "NIL folds")
          (cold-check (= (cold-vsym w (make-vsym "LISP" "T"))
                         (cold-world-t-vma w))
                      "T folds"))
        ;; Lists: cdr-coding, dotted, shared tail, self-reference
        (let ((l (list (make-vsym "SYSTEM" "A") 1 2)))
          (multiple-value-bind (tag data) (cold-ref w l)
            (cold-check (= (tag-type tag) (cold-dtp w "LIST")) "list tag")
            (multiple-value-bind (t0 d0) (cw-ref w data)
              (declare (ignore d0))
              (cold-check (= (ash t0 -6) +cdr-next+) "list cdr-next"))
            (multiple-value-bind (t2 d2) (cw-ref w (+ data 2))
              (cold-check (and (= (ash t2 -6) +cdr-nil+) (= d2 2))
                          "list cdr-nil end"))
            ;; Shared tail: (B . tail-of-l) reuses l's cells
            (let ((l2 (cons (make-vsym "SYSTEM" "B") (cdr l))))
              (multiple-value-bind (tag2 data2) (cold-ref w l2)
                (declare (ignore tag2))
                (multiple-value-bind (t0 d0) (cw-ref w data2)
                  (declare (ignore d0))
                  (cold-check (= (ash t0 -6) +cdr-normal+)
                              "shared-tail car Q is cdr-normal: ~2,'0X" t0))
                (multiple-value-bind (ct cd) (cw-ref w (+ data2 1))
                  (cold-check (and (= (tag-type ct) (cold-dtp w "LIST"))
                                   (= cd (+ data 1)))
                              "shared tail links into first list: ~2,'0X:~8,'0X"
                              ct cd))))))
        (let ((dotted (cons 1 2)))
          (multiple-value-bind (tag data) (cold-ref w dotted)
            (declare (ignore tag))
            (multiple-value-bind (t0 d0) (cw-ref w data)
              (declare (ignore d0))
              (cold-check (= (ash t0 -6) +cdr-normal+) "dotted cdr-normal"))
            (expect-q w (1+ data) (tag 0 fixnum) 2 "dotted tail")))
        (let ((selfref (list 1 2)))
          (setf (second selfref) selfref)
          (multiple-value-bind (tag data) (cold-ref w selfref)
            (declare (ignore tag))
            (multiple-value-bind (t1 d1) (cw-ref w (1+ data))
              (cold-check (and (= (tag-type t1) (cold-dtp w "LIST"))
                               (= d1 data))
                          "self-referential list"))))
        ;; Characters and floats
        (multiple-value-bind (tag data) (cold-ref w (make-vchar 65 0))
          (cold-check (and (= (tag-type tag) (cold-dtp w "CHARACTER"))
                           (= data 65))
                      "character A"))
        (multiple-value-bind (tag data) (cold-ref w (make-vsingle #x3F800000))
          (cold-check (and (= (tag-type tag) (cold-dtp w "SINGLE-FLOAT"))
                           (= data #x3F800000))
                      "single 1.0"))
        (multiple-value-bind (tag data)
            (cold-ref w (make-vdouble #x3FF0000000000000))
          (cold-check (= (tag-type tag) (cold-dtp w "DOUBLE-FLOAT")) "double tag")
          (expect-q w data (tag +cdr-next+ fixnum) #x3FF00000 "double high")
          (expect-q w (1+ data) (tag +cdr-nil+ fixnum) 0 "double low"))
        ;; Locative
        (let ((sym (make-vsym "SYSTEM" "BAR")))
          (multiple-value-bind (tag data)
              (cold-ref w (make-vloc :value sym))
            (cold-check (and (= (tag-type tag) (cold-dtp w "LOCATIVE"))
                             (= data (1+ (cold-vsym w sym))))
                        "value-cell locative")))
        ;; ART-Q array with contents + leader
        (let ((arr (make-varray '(3) (list (make-vsym "KEYWORD" "TYPE")
                                           (make-vsym "SYSTEM" "ART-Q")
                                           (make-vsym "KEYWORD" "FILL-POINTER")
                                           2))))
          (setf (varray-contents arr) (vector 10 20 30))
          (multiple-value-bind (tag header) (cold-ref w arr)
            (cold-check (= (tag-type tag) (cold-dtp w "ARRAY")) "array tag")
            (multiple-value-bind (htag hdata) (cw-ref w header)
              (cold-check (and (= htag (tag 1 (cold-dtp w "HEADER-I")))
                               (= (ldb (byte 6 26) hdata)
                                  (cold-array-type-code w "ART-Q"))
                               (= (ldb (byte 8 15) hdata) 1)  ; leader length
                               (= (ldb (byte 15 0) hdata) 3))
                          "ART-Q header ~2,'0X:~8,'0X" htag hdata))
            (expect-q w (- header 1) (tag 0 fixnum) 2 "fill pointer leader")
            (multiple-value-bind (lt ld) (cw-ref w (- header 2))
              (cold-check (and (= (tag-type lt) (cold-dtp w "HEADER-P"))
                               (= (ash lt -6)
                                  (layout-value (cold-world-layout w)
                                                "SYSTEM:%HEADER-TYPE-LEADER"))
                               (= ld header))
                          "leader header ~2,'0X:~8,'0X" lt ld))
            (expect-q w (+ header 1) (tag 0 fixnum) 10 "array [0]")
            (expect-q w (+ header 3) (tag 0 fixnum) 30 "array [2]")))
        ;; Packed numeric array (ART-16B)
        (let ((arr (make-varray '(3) (list (make-vsym "KEYWORD" "TYPE")
                                           (make-vsym "SYSTEM" "ART-16B")))))
          (setf (varray-words arr) (coerce #(#x1111 #x2222 #x3333)
                                           '(vector (unsigned-byte 16))))
          (multiple-value-bind (tag header) (cold-ref w arr)
            (declare (ignore tag))
            (expect-q w (+ header 1) (tag 0 fixnum) #x22221111 "16b word 0")
            (expect-q w (+ header 2) (tag 0 fixnum) #x3333 "16b word 1")))))))

;;; M3b census: every non-vop object in a decoded file materializes.

(defun cold-materialize-value (w value)
  "Materialize VALUE if cold-ref supports it; recurse into vops.  Compiled
functions, eval forms and instances wait for later stages; a list carrying
one falls back to materializing its leaves individually (test aid only)."
  (typecase value
    ((or veval vembed vnative vpackage vcharset null) nil)
    (vop (mapc (lambda (v) (cold-materialize-value w v)) (vop-args value)))
    (cons (handler-case (cold-ref w value)
            (error ()
              (loop for c = value then (cdr c)
                    while (consp c)
                    do (cold-materialize-value w (car c))
                    finally (when c (cold-materialize-value w c))))))
    (t (cold-ref w value))))

(defun check-vbin-census (w path)
  "Decode PATH and materialize every event object; interned-symbol count in
the cold world matches a host-side count of distinct (pname, package)."
  (with-cold-checks ("cold vbin census")
    (let ((vbin (read-vbin path))
          (host-symbols (make-hash-table :test #'equal))
          (errors 0))
      (let ((*cold-default-package* "SYSTEM-INTERNALS"))
        ;; Host-side reference count of interned symbols
        (labels ((visit (v)
                   (typecase v
                     (vsym (let ((pkg (canonical-package-name (vsym-package v))))
                             (when (and pkg
                                        (not (member (vsym-name v) '("NIL" "T")
                                                     :test #'string=)))
                               (setf (gethash (cons (vsym-name v)
                                                    (cold-resolve-home
                                                     (vsym-name v) pkg))
                                              host-symbols)
                                     t))))
                     (vop (mapc #'visit (vop-args v)))
                     (vloc (visit (vloc-target v)))
                     ;; Mirror cold-fun: operands only; the fspec is interned
                     ;; by the FDEFINE vop, not by function materialization.
                     (vfun (map nil (lambda (vw)
                                      (unless (logbitp 9 (vword-op vw))
                                        (visit (vword-data vw))))
                                (vfun-words v)))
                     (veval nil)
                     (vinstance
                      ;; Materialization interns the marker + its variable.
                      (setf (gethash (cons "COLD-INSTANCE-MARKER" nil)
                                     host-symbols)
                            t
                            (gethash (cons "*COLD-MAKE-INSTANCE-MARKER*"
                                           (cold-resolve-home
                                            "*COLD-MAKE-INSTANCE-MARKER*"
                                            "SYSTEM-INTERNALS"))
                                     host-symbols)
                            t)
                      (visit (vinstance-flavor v))
                      (visit (vinstance-plist v)))
                     (cons (loop for c = v then (cdr c)
                                 while (consp c)
                                 do (visit (car c))
                                 finally (when c (visit c))))
                     (varray (when (varray-contents v)
                               (map nil #'visit (varray-contents v)))))))
          (loop for (op . value) in (vbin-file-events vbin)
                do (visit value)))
        (with-cold-materializer (w)
          (let ((*cold-load-time-eval*
                  ;; Census parity: host-side visit skips veval forms too.
                  (lambda (w form)
                    (declare (ignore form))
                    (cold-nil-q w))))
            (loop for (op . value) in (vbin-file-events vbin)
                  do (handler-case (cold-materialize-value w value)
                       (error (e)
                         (incf errors)
                         (when (<= errors 3)
                           (cold-check nil "materialize ~S: ~A"
                                       (type-of value) e)))))))
        (cold-check (zerop errors) "~D materialization errors" errors)
        (cold-check (= (hash-table-count (cold-world-symbols w))
                       (hash-table-count host-symbols))
                    "symbol census: cold ~D vs host ~D"
                    (hash-table-count (cold-world-symbols w))
                    (hash-table-count host-symbols))))))

;;; M3c: compiled functions

(defun walk-vfuns (value fn &optional (seen (make-hash-table :test #'eq)))
  "Call FN on every vfun reachable inside VALUE (events, vops, lists,
arrays, nested vword operands)."
  (labels ((visit (v)
             (typecase v
               (vfun (unless (gethash v seen)
                       (setf (gethash v seen) t)
                       (funcall fn v)
                       (map nil (lambda (vw) (visit (vword-data vw)))
                            (vfun-words v))))
               (vop (mapc #'visit (vop-args v)))
               (cons (loop for c = v then (cdr c)
                           while (consp c)
                           do (visit (car c))
                           finally (when c (visit c))))
               (varray (when (varray-contents v)
                         (map nil #'visit (varray-contents v)))))))
    (visit value)))

(defun check-cold-set-vfuns (w sysdir)
  "M3c gate: every compiled function in the whole cold set materializes."
  (declare (ignore sysdir))
  (with-cold-checks ("cold vfuns (full cold set)")
    (let ((files 0) (fns 0) (errors 0) (evals 0) (first-errors nil))
      (with-cold-materializer (w)
        (let ((*cold-load-time-eval*
                ;; Eval-at-load-time operands are M3d's gate; count them and
                ;; substitute NIL so the sweep can cover every function.
                (lambda (w form)
                  (declare (ignore form))
                  (incf evals)
                  (cold-nil-q w))))
          (dolist (spec *cold-load-order*)
            (let ((vbin (read-vbin (sys-pathname spec))))
              (incf files)
              (dolist (event (vbin-file-events vbin))
                (walk-vfuns (cdr event)
                            (lambda (vf)
                              (incf fns)
                              (handler-case (cold-fun w vf)
                                (error (e)
                                  (incf errors)
                                  (when (< (length first-errors) 5)
                                    (push (format nil "~A: ~S: ~A" spec
                                                  (first (vfun-name-and-storage vf))
                                                  e)
                                          first-errors)))))))))))
      (dolist (msg (reverse first-errors)) (cold-check nil "~A" msg))
      (cold-check (zerop errors) "~D of ~D vfuns failed (~D files)"
                  errors fns files)
      (when (zerop errors)
        (format t "  ~D files, ~D compiled functions, ~D eval-operands deferred~%"
                files fns evals)))))

(defun find-fdefine-vfun (vbin fspec-name)
  "The vfun whose fspec is the symbol named FSPEC-NAME in VBIN's events
(fdefines may hide inside LOAD-MULTIPLE-DEFINITION eval forms, so match on
the vfun's own name-and-storage)."
  (loop for (op . value) in (vbin-file-events vbin)
        do (let ((found nil))
             (declare (ignorable op))
             (walk-vfuns value
                         (lambda (vf)
                           (let ((fspec (first (vfun-name-and-storage vf))))
                             (when (and (vsym-p fspec)
                                        (string= (vsym-name fspec) fspec-name))
                               (setf found vf)))))
             (when found (return found)))))

(defun check-system-startup-oracle (w reference)
  "M3c oracle: materialize SYSTEM-STARTUP from wired.vbin and compare it
Q-for-Q with the distribution world's copy at the address SYSCOM slot 2
points to (1C:F8046C44 in genera-8-5-wired.txt)."
  (with-cold-checks ("cold SYSTEM-STARTUP oracle")
    (let* ((vbin (read-vbin (sys-pathname "SYS: SYS; WIRED")))
           (vf (find-fdefine-vfun vbin "SYSTEM-STARTUP")))
      (cold-check vf "SYSTEM-STARTUP fdefine found in wired.vbin")
      (when vf
        (multiple-value-bind (sstag ssdata)
            (world-q reference #xF8041102)      ; SYSCOM systemStartup slot
          (cold-check (and sstag (= (tag-type sstag)
                                    (cold-dtp w "COMPILED-FUNCTION")))
                      "reference systemStartup slot is a compiled function")
          (let* ((ref-fn ssdata)
                 (fn (with-cold-materializer (w) (cold-fun w vf)))
                 (total (vfun-total-size vf))
                 (suffix (vfun-suffix-size vf))
                 (n-instr (- total suffix 2))
                 (mismatches 0))
            ;; CCA header of the reference copy
            (multiple-value-bind (htag hdata) (world-q reference (- ref-fn 2))
              (cold-check (and htag
                               (= hdata (logior (ash suffix 18) total)))
                          "reference CCA header ~8,'0X vs total/suffix ~D/~D"
                          hdata total suffix))
            ;; Instruction Qs: tags identical; immediates identical;
            ;; relative operands identical modulo base.
            (dotimes (i n-instr)
              (let* ((vw (aref (vfun-words vf) i))
                     (op (vword-op vw))
                     (imm (logbitp 9 op))
                     (rel (logbitp 10 op))
                     ;; Word 0's CURRENT-DEFINITION-P (bit 28) is set by
                     ;; FDEFINE, not by function loading; the generator's
                     ;; fdefine handling supplies it (M3d).
                     (mask (if (zerop i) #xEFFFFFFF #xFFFFFFFF)))
                (multiple-value-bind (gtag gdata) (cw-ref w (+ fn i))
                  (multiple-value-bind (rtag rdata)
                      (world-q reference (+ ref-fn i))
                    (unless (and rtag
                                 (= gtag rtag)
                                 (cond (rel (= (- gdata fn) (- rdata ref-fn)))
                                       (imm (= (logand gdata mask)
                                               (logand rdata mask)))
                                       (t t)))
                      (incf mismatches)
                      (when (<= mismatches 3)
                        (cold-check nil
                                    "instr ~D: ours ~2,'0X:~8,'0X ref ~A:~A"
                                    i gtag gdata
                                    (and rtag (format nil "~2,'0X" rtag))
                                    (and rtag (format nil "~8,'0X" rdata)))))))))
            (cold-check (zerop mismatches)
                        "~D instruction mismatches (of ~D)" mismatches n-instr)
            ;; Entry-PC formula on both copies gives the same offset.
            (let ((ours (- (cold-fun-entry-pc w fn) fn)))
              (multiple-value-bind (rt rd) (world-q reference ref-fn)
                (declare (ignore rt))
                (cold-check (= ours (1+ (- (ldb (byte 8 18) rd)
                                           (ldb (byte 8 0) rd))))
                            "entry-pc offset ~D matches reference" ours)))))))))

;;; M3d: vop dispatcher + mini-eval over the whole cold set

(defun cold-eval-stats-report ()
  (let ((rows nil))
    (maphash (lambda (k v) (push (cons k v) rows)) *cold-eval-stats*)
    (dolist (r (sort rows (lambda (a b)
                            (if (= (cdr a) (cdr b))
                                (string< (car a) (car b))
                                (> (cdr a) (cdr b))))))
      (format t "  ~6D  ~A~%" (cdr r) (car r)))))

;;; Boot-time callability of the deferred list.  MAPC #'EVAL runs it before
;;; the banner (cold-load.lisp:547) with only the cold fdefinitions plus the
;;; LISP-INITIALIZE-FIRST-TIME stubs installed; any other head crashes boot.

(defparameter *cold-boot-stub-functions*
  '("SPECIAL-LOAD" "DEFCONSTANT-LOAD-2" "DEFGENERIC-INTERNAL"
    "REDEFINE-FORMAT-DIRECTIVE" "MAKE-VARIABLE-OBSOLETE" "SUBTYPEP"
    "WARN" "FERROR" "ERROR" "MAKE-INSTANCE" "FIND-PACKAGE" "FIND-CLASS"
    ;; GLOBAL:FORMAT is unbound by design since the boot-15 dialect
    ;; split (LISP:FORMAT carries the CLCP wrapper); FORMAT-COLD-LOAD
    ;; installs on it at FSET time.
    "FORMAT"
    ;; FSET alist cold-load.lisp:203: the -COLD stub conses a
    ;; (*COLD-FIND-GENERIC-FUNCTION-MARKER* name) list that QLD's
    ;; BOOTSTRAP-DEFGENERIC-CONSTANT-REFERENCES snaps
    ;; (flavor/bootstrap.lisp:75); the marker symbol is a generator
    ;; stamp (M3h boot 28).
    "FIND-GENERIC-FUNCTION-AS-CONSTANT")
  "Stubbed by *COLD-LOAD-FUNCTION-INITIALIZATIONS* (cold-load.lisp:131).")

(defparameter *cold-known-pending-functions*
  '()
  "Heads accepted by the audit despite having no cold definition or stub,
so the gate tracks only NEW problems.  Empty since the three late-found
cold files (LISP-DATABASE-COLD, ITRAP-DISPATCH, IGC-COLD) joined the
load order.")

(defparameter *cold-interpreter-special-forms*
  '("IF" "PROGN" "QUOTE" "SETQ" "LET" "LET*" "COND" "AND" "OR" "BLOCK"
    "RETURN-FROM" "TAGBODY" "GO" "PROG1" "PROG2" "FUNCTION" "THE"
    "MULTIPLE-VALUE-BIND" "LAMBDA" "DECLARE")
  "Digested natively by the cold interpreter (sys/eval.lisp); no function
cell needed.")

(defun check-deferred-boot-safety (w)
  "Walk every deferred form AND every first-boot patch value form; flag
call heads that are neither cold-defined, stub-backed, FBOUNDP-guarded,
nor interpreter special forms.  Patches run in the same pre-banner MAPC
\(spine = patches ++ deferred ++ relative-names), so their value forms
have exactly the same callability obligation -- boot 28 trapped on a
patch calling SI:DEFSELECT-CONS-WHICH-OPERATIONS (a plain-DEFSELECT
eval-at-load-time operand, ldefsel.lisp:143) before ldefsel joined the
cold set."
  (let ((bad (make-hash-table :test #'equal)))
    (loop for (pkg . form) in
          (append (cold-world-deferred w)
                  ;; Guarded-head patches materialize behind
                  ;; (IF (FBOUNDP ...)) and never run pre-banner --
                  ;; same exemption GUARDP gives deferred forms.
                  (loop for p in (cold-world-patches w)
                        for pform = (third p)
                        unless (and (consp pform) (vsym-p (first pform))
                                    (member (vsym-name (first pform))
                                            *cold-guarded-patch-heads*
                                            :test #'string=))
                          collect (cons (second p) pform)))
          do (let ((*cold-default-package* pkg))
               (labels
                   ((callable-p (v)
                      (let ((name (vsym-name v)))
                        (or (member name *cold-interpreter-special-forms*
                                    :test #'string=)
                            (member name *cold-boot-stub-functions*
                                    :test #'string=)
                            (member name *cold-known-pending-functions*
                                    :test #'string=)
                            (gethash (fspec-key v) (cold-world-fdefs w)))))
                    (guardp (f)
                      ;; (IF (FBOUNDP 'head) body): body exempt.
                      (and (consp f) (vsym-named-p (first f) "IF")
                           (consp (second f))
                           (vsym-named-p (first (second f)) "FBOUNDP")))
                    (walk (f)
                      (when (consp f)
                        (cond
                          ((vsym-named-p (first f) "QUOTE"))
                          ;; (FUNCTION fspec): the fspec is data, and list
                          ;; fspecs like (:PROPERTY ...) are not calls.
                          ((vsym-named-p (first f) "FUNCTION"))
                          ((guardp f))
                          ((or (vsym-named-p (first f) "LET")
                               (vsym-named-p (first f) "LET*"))
                           ;; bindings are (var value) pairs, not calls
                           (dolist (b (second f))
                             (when (consp b) (walk (second b))))
                           (mapc #'walk (cddr f)))
                          ((vsym-named-p (first f) "TAGBODY")
                           (dolist (s (rest f))
                             (when (consp s) (walk s))))
                          (t
                           (when (vsym-p (first f))
                             (unless (callable-p (first f))
                               (setf (gethash (vsym-name (first f)) bad) t)))
                           (loop for sub = (rest f) then (cdr sub)
                                 while (consp sub)
                                 do (walk (car sub))))))))
                 (walk form))))
    (let ((names (sort (loop for k being the hash-keys of bad collect k)
                       #'string<)))
      (cold-check (null names)
                  "deferred forms call undefined-at-boot: ~{~A~^ ~}" names))))

(defun check-cold-eval (w reference)
  "M3d gate: full 88-file load with zero unhandled forms and zero
unresolved fixups; register-map ASETs took; the trap page carries real
vectors and ITRAP-DISPATCH's entry-T catch-all displaced the synthesized
filler; SYSTEM-STARTUP is Q-for-Q EXACT against the reference
(CURRENT-DEFINITION-P included, courtesy of the fdefine handler)."
  (with-cold-checks ("cold eval (full cold set)")
    (let ((*cold-eval-stats* (make-hash-table :test #'equal))
          (failures nil)
          (fixup-failures 0)
          (skeleton-catch-all (cold-world-catch-all-pc w)))
      (handler-case
          (setf fixup-failures (cold-load-cold-set w))
        (error (e) (push (format nil "~A" e) failures)))
      (dolist (msg failures) (cold-check nil "load error: ~A" msg))
      (cold-check (zerop fixup-failures)
                  "~D unresolved fixups" fixup-failures)
      (cold-check (/= (cold-world-catch-all-pc w) skeleton-catch-all)
                  "trap catch-all still the synthesized filler (entry-T ~
SET-TRAP-VECTOR-ENTRY from ITRAP-DISPATCH never ran)")
      (format t "  mini-eval actions:~%")
      (cold-eval-stats-report)
      (format t "  deferred forms: ~D, patches: ~D, magic blocks: ~D~%"
              (length (cold-world-deferred w))
              (length (cold-world-patches w))
              (length (cold-world-magic w)))
      (when (null failures)
        (check-deferred-boot-safety w)
        ;; ASET spot check: the readable register map has symbol entries.
        (multiple-value-bind (tag data boundp)
            (cold-symbol-value-q
             w (make-vsym "SYSTEM" "*INTERNAL-READABLE-REGISTER-MAP*"))
          (cold-check (and boundp (= (tag-type tag) (cold-dtp w "ARRAY")))
                      "readable register map is a bound array")
          (when boundp
            (let ((symbols 0))
              (multiple-value-bind (ht hd) (cw-ref w data)
                (declare (ignore ht))
                (dotimes (i (ldb (byte 15 0) hd))
                  (multiple-value-bind (et ed) (cw-ref w (+ data 1 i))
                    (declare (ignore ed))
                    (when (= (tag-type et) (cold-dtp w "SYMBOL"))
                      (incf symbols)))))
              (cold-check (> symbols 50)
                          "register map has ~D symbol entries" symbols))))
        ;; Trap page: a healthy population of explicit vectors.
        (let ((trap-base (cold-address w "%TRAP-VECTOR-BASE"))
              (catch-all (cold-world-catch-all-pc w))
              (explicit 0))
          (dotimes (i (layout-value (cold-world-layout w)
                                    "%TRAP-VECTOR-LENGTH"))
            (multiple-value-bind (tag data) (cw-ref w (+ trap-base i))
              (when (and (= (tag-type tag) (cold-dtp w "EVEN-PC"))
                         (/= data catch-all))
                (incf explicit))))
          (cold-check (> explicit 500)
                      "trap page: only ~D explicit vectors" explicit))
        ;; SYSTEM-STARTUP oracle, now exact (no bit-28 mask).
        (when reference
          (let ((cell (gethash (fspec-key (make-vsym "SYSTEM-INTERNALS"
                                                     "SYSTEM-STARTUP"))
                               (cold-world-fdefs w))))
            (cold-check cell "SYSTEM-STARTUP was fdefined")
            (when cell
              (multiple-value-bind (tag fn)
                  (cw-ref w (cold-follow-cell w cell))
                (cold-check (= (tag-type tag)
                               (cold-dtp w "COMPILED-FUNCTION"))
                            "SYSTEM-STARTUP cell holds a function")
                ;; The M3c oracle already matched every instruction modulo
                ;; the FDEFINE bit; here the entry instruction must be
                ;; EXACT -- fdefine set CURRENT-DEFINITION-P (bit 28).
                (multiple-value-bind (rt ref-fn) (world-q reference #xF8041102)
                  (declare (ignore rt))
                  (multiple-value-bind (gt gd) (cw-ref w fn)
                    (multiple-value-bind (rt2 rd) (world-q reference ref-fn)
                      (cold-check (logbitp 28 gd)
                                  "CURRENT-DEFINITION-P set on entry instruction")
                      (cold-check (and rt2 (= gt rt2) (= gd rd))
                                  "entry instruction exact: ~2,'0X:~8,'0X vs ~2,'0X:~8,'0X"
                                  gt gd rt2 rd))))))))))))

;;; M3e: wired machinery

;;; Ground-truth addresses of the generator's first allocations (probe of
;;; Genera-8-5.vlod, 2026-07-04).  Deterministic because both generators
;;; reserve them first in their regions.
(defparameter *cold-machinery-table-addresses*
  '((:region-free-pointer             #xF804120D)
    (:region-gc-pointer               #xF804160E)
    (:region-quantum-origin           #xF8041A0F)
    (:region-quantum-length           #xF8041E10)
    (:region-bits                     #xF8042211)
    (:area-name                       #xF0000002)
    (:area-maximum-quantum-size       #xF0000085)
    (:area-region-quantum-size        #xF0000108)
    (:area-region-list                #xF000018B)
    (:area-region-bits                #xF00001CE)
    (:region-free-pointer-before-flip #xF000024F)
    (:region-list-thread              #xF0000650)
    (:region-created-pages            #xF0000851)
    (:region-area                     #xF0000C52)
    (:oblast-free-size                #xF0000E53)))

;;; Per-slot expectations for the SYSCOM block in a COLD world.  Slots the
;;; FEP or first boot owns (address-space map, PHT windows, sysout, package
;;; table) are only required to be unbound/NIL/loose; generator-owned slots
;;; are exact.  :TABLE slots must hold the machinery array -- which is also
;;; Q-for-Q the reference value, since the addresses match.
(defparameter *cold-syscom-slot-spec*
  '((0 :fixnum= 452) (1 :fixnum= 22) (2 :tag "COMPILED-FUNCTION")
    (3 :unset) (4 :table :oblast-free-size) (5 :table :area-name)
    (6 :table :area-maximum-quantum-size)
    (7 :table :area-region-quantum-size) (8 :table :area-region-list)
    (9 :table :area-region-bits) (10 :table :region-quantum-origin)
    (11 :table :region-quantum-length) (12 :table :region-free-pointer)
    (13 :table :region-gc-pointer) (14 :table :region-bits)
    (15 :table :region-list-thread) (16 :table :region-area)
    (17 :table :region-created-pages)
    (18 :table :region-free-pointer-before-flip)
    (19 :fixnum= 0) (20 :fixnum= 0) (21 :unset) (22 :unset)
    (23 :fixnum= 8)
    (24 :unset) (25 :unset) (26 :unset) (27 :unset) (28 :unset) (29 :unset)
    (30 :unset) (31 :unset) (32 :unset) (33 :unset) (34 :unset) (35 :unset)
    (36 :t) (37 :unset) (38 :unset) (39 :unset)
    (40 :fixnum= #x4A600) (41 :fixnum= #xF804A600)
    (42 :unset) (43 :loose) (44 :loose) (45 :loose) (46 :loose) (47 :loose)
    (48 :initial-sg) (49 :unset) (50 :unset) (51 :fixnum= #x240) (52 :t)
    (53 :locative= #xF6000000) (54 :loose) (55 :fixnum) (56 :fixnum= 0)
    (57 :unset) (58 :tag "STRING") (59 :unset)))

(defun cold-magic-block (w suffix)
  (or (find-if (lambda (m) (string= (vsym-name (first m)) suffix))
               (cold-world-magic w))
      (error "No magic block ~A was stashed" suffix)))

(defun check-magic-forwarding (w layout-block stash)
  "Every variable of a comm block: stash matches the layout ventries and
the symbol's cell is a one-q-forward to its slot."
  (destructuring-bind (block-name start end ventries) layout-block
    (declare (ignore end))
    (let ((vars (third stash))
          (fwd (cold-dtp w "ONE-Q-FORWARD")))
      (cold-check (= (length vars) (length ventries))
                  "~A: stash has ~D vars, layout ~D"
                  block-name (length vars) (length ventries))
      (loop for var in vars
            for (vtype vval) in ventries
            for slot from start
            do (multiple-value-bind (cell kind) (cold-magic-var-cell w var)
                 (cold-check (eq kind (if (eq vtype :function) :function :value))
                             "~A slot #x~8,'0X: stash kind ~S vs layout ~S"
                             block-name slot kind vtype)
                 (let ((lname (strip-package (second vval)))
                       (sname (vsym-name (if (consp var) (second var) var))))
                   (cold-check (string= lname sname)
                               "~A slot #x~8,'0X: stash ~A vs layout ~A"
                               block-name slot sname lname))
                 (multiple-value-bind (tag data) (cw-ref w cell)
                   (cold-check (and (= (tag-type tag) fwd) (= data slot))
                               "~A ~A: cell #x~8,'0X is ~2,'0X:~8,'0X, not a ~
forward to #x~8,'0X"
                               block-name
                               (vsym-name (if (consp var) (second var) var))
                               cell tag data slot)))))))

(defun check-syscom-slots (w start)
  (let ((dtp-null (cold-dtp w "NULL"))
        (dtp-nil (cold-dtp w "NIL"))
        (fixnum (cold-dtp w "FIXNUM")))
    (loop for (index kind value) in *cold-syscom-slot-spec*
          for slot = (+ start index)
          do (multiple-value-bind (tag data) (cw-ref w slot)
               (let ((type (tag-type tag)))
                 (flet ((fail (want)
                          (cold-check nil "SYSCOM+~D: ~A expected, got ~
~2,'0X:~8,'0X" index want tag data)))
                   (ecase kind
                     (:fixnum= (unless (and (= type fixnum) (= data value))
                                 (fail (format nil "fixnum ~D" value))))
                     (:fixnum (unless (= type fixnum) (fail "a fixnum")))
                     (:locative= (unless (and (= type (cold-dtp w "LOCATIVE"))
                                              (= data value))
                                   (fail (format nil "locative #x~X" value))))
                     (:tag (unless (= type (cold-dtp w value))
                             (fail (format nil "tag DTP-~A" value))))
                     (:t (unless (and (= type (cold-dtp w "SYMBOL"))
                                      (= data (cold-world-t-vma w)))
                           (fail "T")))
                     (:unset (unless (or (= type dtp-null) (= type dtp-nil))
                               (fail "unbound or NIL")))
                     (:loose (unless (or (= type dtp-null) (= type dtp-nil)
                                         (= type fixnum))
                               (fail "unbound, NIL or fixnum")))
                     (:table (unless (and (= type (cold-dtp w "ARRAY"))
                                          (= data (cold-machinery w value)))
                               (fail (format nil "machinery table ~S" value))))
                     (:initial-sg
                      (unless (and (= type (cold-dtp w "ARRAY"))
                                   (= data (cold-machinery
                                            w :initial-stack-group)))
                        (fail "the initial stack group"))))))))))

(defun check-machinery-region-tables (w)
  "The emitted tables describe this world's regions, verified by
independent reconstruction."
  (let ((regions (cold-world-regions w))
        (org-tbl (cold-machinery w :region-quantum-origin))
        (len-tbl (cold-machinery w :region-quantum-length))
        (fp-tbl (cold-machinery w :region-free-pointer))
        (thread-tbl (cold-machinery w :region-list-thread))
        (area-tbl (cold-machinery w :region-area))
        (rlist-tbl (cold-machinery w :area-region-list)))
    (loop for region across regions
          for r = (cold-region-number region)
          do (multiple-value-bind (tag data) (cw-ref w (+ org-tbl 1 r))
               (declare (ignore tag))
               (cold-check (= data (ash (cold-region-origin region) -16))
                           "region ~D origin quantum ~X vs table ~X"
                           r (ash (cold-region-origin region) -16) data))
             (multiple-value-bind (tag data) (cw-ref w (+ len-tbl 1 r))
               (declare (ignore tag))
               (cold-check (= data (ceiling (cold-region-length region)
                                            #x10000))
                           "region ~D quantum length" r))
             (multiple-value-bind (tag data) (cw-ref w (+ fp-tbl 1 r))
               (declare (ignore tag))
               (cold-check (= data (- (cold-region-free region)
                                      (cold-region-origin region)))
                           "region ~D free pointer" r))
             (cold-check (= (cold-table-ref16 w area-tbl r)
                            (cold-region-area region))
                         "region ~D area" r))
    ;; Area chains: rlist -> thread ... -1 reproduces cold-area-regions.
    (loop for area across (cold-world-areas w)
          when area
            do (let ((chain nil)
                     (r (cold-table-ref16 w rlist-tbl (cold-area-number area))))
                 (loop for guard from 0 below 64
                       until (= r #xFFFF)
                       do (push r chain)
                          (setf r (cold-table-ref16 w thread-tbl r)))
                 (cold-check (equal (reverse chain) (cold-area-regions area))
                             "area ~D chain ~S vs regions ~S"
                             (cold-area-number area) (reverse chain)
                             (cold-area-regions area))))
    ;; The wired region stayed inside the architectural limit.
    (let ((wired (aref regions 1)))
      (cold-check (<= (cold-region-free wired) +wired-zone-limit+)
                  "wired region free #x~8,'0X exceeds #x~8,'0X"
                  (cold-region-free wired) +wired-zone-limit+))))

(defun check-initial-stack-group (w)
  (let* ((layout (cold-world-layout w))
         (sg (cold-machinery w :initial-stack-group))
         (locative (cold-dtp w "LOCATIVE")))
    (flet ((field (name)
             (multiple-value-bind (tag data)
                 (cw-ref w (+ sg (cold-sg-field-word layout name)))
               (values (tag-type tag) data))))
      (multiple-value-bind (tag data) (cw-ref w sg)
        (cold-check (and (= (tag-type tag) (cold-dtp w "HEADER-I"))
                         (logbitp 25 data)
                         (= (ldb (byte 15 0) data) 63))
                    "SG header named-structure ART-Q 63, got ~2,'0X:~8,'0X"
                    tag data))
      (multiple-value-bind (tag data) (cw-ref w (+ sg 1))
        (declare (ignore data))
        (cold-check (= (tag-type tag) (cold-dtp w "SYMBOL"))
                    "SG element 0 holds the STACK-GROUP symbol"))
      (multiple-value-bind (type data) (field "SG-STATUS-BITS")
        (declare (ignore type))
        (cold-check (= data +cold-sg-status+)
                    "SG status #x~X (uninitialized+safe = #x~X)"
                    data +cold-sg-status+))
      (multiple-value-bind (type data) (field "SG-CONTROL-STACK-LOW")
        (cold-check (and (= type locative) (= data #xF6000000))
                    "SG control-stack-low"))
      (multiple-value-bind (type data) (field "SG-CONTROL-STACK-LIMIT")
        (cold-check
         (and (= type locative)
              (= data (- (+ #xF6000000 +cold-control-stack-size+)
                         (layout-value layout "CONTROL-STACK-OVERFLOW-MARGIN")
                         (layout-value layout "CONTROL-STACK-MAX-FRAME-SIZE"))))
         "SG control-stack-limit #x~8,'0X" data))
      (multiple-value-bind (type data) (field "SG-BINDING-STACK-POINTER")
        (cold-check (and (= type locative) (= data #xF2000001))
                    "SG binding-stack-pointer"))
      (multiple-value-bind (type data) (field "SG-BINDING-STACK-LIMIT")
        (cold-check (and (= type locative)
                         (= data (+ #xF2000000 +cold-binding-stack-size+ -1)))
                    "SG binding-stack-limit"))
      (multiple-value-bind (type data) (field "SG-DATA-STACK-LOW")
        (declare (ignore data))
        (cold-check (= type (cold-dtp w "NIL"))
                    "SG data stack unallocated (GROW-DATA-STACK owns it)"))
      ;; Stack pages exist.
      (cold-check (nth-value 2 (cw-ref w #xF6000000)) "control stack pages")
      (cold-check (nth-value 2 (cw-ref w #xF2000000)) "binding stack pages"))))

(defun check-trap-page-against-reference (w reference)
  "After the grafts, tags may differ from the reference only in the
generic-dispatch block (warm-installed)."
  (multiple-value-bind (labels base len) (trap-vector-labels
                                          (cold-world-layout w))
    (declare (ignore labels))
    (let ((unexpected nil))
      (dotimes (i len)
        (multiple-value-bind (gt gd) (cw-ref w (+ base i))
          (declare (ignore gd))
          (multiple-value-bind (rt rd) (world-q reference (+ base i))
            (declare (ignore rd))
            (when (and rt (/= gt rt))
              (cond ((<= 2560 i 2623))           ; generic dispatch, warm
                    (t (push i unexpected)))))))
      (cold-check (null unexpected)
                  "trap page: unexpected tag mismatches at ~S" unexpected)
      ;; Grafted slots are Q-for-Q the reference.
      (dolist (slot *cold-ifep-vector-slots*)
        (multiple-value-bind (gt gd) (cw-ref w (+ base slot))
          (multiple-value-bind (rt rd) (world-q reference (+ base slot))
            (cold-check (and (= gt rt) (= gd rd))
                        "graft slot ~D: ~2,'0X:~8,'0X vs ref ~2,'0X:~8,'0X"
                        slot gt gd rt rd)))))))

(defun check-magic-table-vs-reference (w reference start)
  "The table-valued SYSCOM slots are Q-for-Q the reference (same addresses
by construction)."
  (loop for (index kind value) in *cold-syscom-slot-spec*
        when (eq kind :table)
          do (multiple-value-bind (gt gd) (cw-ref w (+ start index))
               (multiple-value-bind (rt rd) (world-q reference (+ start index))
                 (cold-check (and rt (= gt rt) (= gd rd))
                             "SYSCOM+~D table slot ~2,'0X:~8,'0X vs ref ~
~2,'0X:~8,'0X (~S)" index gt gd rt rd value))))
  ;; fepStartup must NOT read as a compiled function, so the emulator
  ;; falls through to systemStartup (interfac.c:775); the M3g FEPComm
  ;; grafts (checked below) must never grow to cover slot 2.
  (multiple-value-bind (tag data) (cw-ref w (+ #xF8041000 2))
    (declare (ignore data))
    (cold-check (/= (tag-type tag) (cold-dtp w "COMPILED-FUNCTION"))
                "FEPCOM fepStartup must not be a compiled function")))

(defun check-fepcomm-grafts (w reference)
  "M3g gate: the 19 grafted FEPComm function slots (FEP-COMMAND-STRING ..
FEP-SEQUENCE-BREAK, slots #x1F-#x31) are Q-for-Q the reference and carry
IFEP debugger-kernel entries (1C:F801xxxx)."
  (destructuring-bind (name base end ventries) (cold-fepcomm-block w)
    (declare (ignore name end ventries))
    (let ((dtp-cf (cold-dtp w "COMPILED-FUNCTION")))
      (loop for fname in *cold-fepcomm-graft-names*
            for slot from +cold-fepcomm-graft-start+
            do (multiple-value-bind (gt gd) (cw-ref w (+ base slot))
                 (multiple-value-bind (rt rd)
                     (world-q reference (+ base slot))
                   (cold-check (and rt (= gt rt) (= gd rd))
                               "FEPComm ~A: ~2,'0X:~8,'0X vs ref ~
~:[unmapped~;~:*~2,'0X:~8,'0X~]" fname gt gd rt rd)
                   (cold-check (and (= (tag-type gt) dtp-cf)
                                    (<= #xF8010000 gd #xF801FFFF))
                               "FEPComm ~A: ~2,'0X:~8,'0X not an IFEP ~
kernel entry" fname gt gd)))))))

(defun check-fepcomm-boot-stamps (w reference)
  "M3h gate: the FEP-populated boot-parameter slots carry the
distribution Qs (the IFEP reads them at startup and halts silently when
they are unbound)."
  (destructuring-bind (bname base end ventries) (cold-fepcomm-block w)
    (declare (ignore bname end))
    (loop for (sname nil nil) in *cold-fepcomm-boot-stamps*
          for slot = (position sname ventries
                               :key (lambda (v)
                                      (strip-package (second (second v))))
                               :test #'string=)
          do (cold-check slot "FEPComm ventry ~A exists" sname)
             (when slot
               (multiple-value-bind (gt gd) (cw-ref w (+ base slot))
                 (multiple-value-bind (rt rd) (world-q reference (+ base slot))
                   (cold-check (and rt (= (tag-type gt) (tag-type rt))
                                    (= gd rd))
                               "FEPComm ~A: ~2,'0X:~8,'0X vs ref ~
~:[unmapped~;~:*~2,'0X:~8,'0X~]" sname gt gd rt rd)))))))

(defun reference-symbol-value (reference name)
  "(values tag data) of NAME's value in the reference world, following
the cell forward; NIL when the symbol is missing.  Prefers a bound cell
when the pname appears in more than one package."
  (loop with best-tag = nil and best-data = nil
        for vma in (world-find-symbols reference name)
        do (multiple-value-bind (tag data) (w-follow-cell reference (1+ vma))
             (when (and tag (or (null best-tag) (/= (tag-type tag) 0)))
               (setf best-tag tag best-data data)))
        finally (return (values best-tag best-data))))

(defun check-wired-arrays (w reference)
  "M3h gate: every generator-owned wired array is bound to a
WIRED-CONTROL-TABLES array whose header Q equals the reference's (same
type/leader/length/dims), leaders match the spec, and the data is
exactly what the spec bakes (NIL / fixnum fill / verbatim words)."
  (let ((array (cold-dtp w "ARRAY"))
        (header-i (cold-dtp w "HEADER-I"))
        (fixnum (cold-dtp w "FIXNUM")))
    (multiple-value-bind (ntag ndata) (cold-nil-q w)
      (dolist (spec *cold-wired-arrays*)
        (destructuring-bind (package name type dims
                             &key fill-pointer leader-length leader-list
                                  contents words fill-fixnum last-cdr-nil
                                  (area "WIRED-CONTROL-TABLES"))
            spec
          (declare (ignore leader-length))
          (multiple-value-bind (tag data boundp)
              (cold-symbol-value-q w (make-vsym package name))
            (let ((region (cold-area-current-region w area)))
              (cold-check (and boundp (= (tag-type tag) array) region
                               (<= (cold-region-origin region) data)
                               (< data (cold-region-free region)))
                          "~A is a bound array in ~A" name area))
            (when (and boundp (= (tag-type tag) array))
              (multiple-value-bind (ht hd) (cw-ref w data)
                (cold-check (= (tag-type ht) header-i)
                            "~A header is HEADER-I" name)
                (multiple-value-bind (rt rd)
                    (reference-symbol-value reference name)
                  (cold-check (and rt (= (tag-type rt) array))
                              "~A bound to an array in the reference" name)
                  (when (and rt (= (tag-type rt) array))
                    (multiple-value-bind (rht rhd) (world-q reference rd)
                      (declare (ignore rht))
                      (cold-check (eql hd rhd)
                                  "~A header ~8,'0X vs ref ~@[~8,'0X~]"
                                  name hd rhd)))))
              (when fill-pointer
                (multiple-value-bind (ft fd) (cw-ref w (- data 1))
                  (cold-check (and (= (tag-type ft) fixnum)
                                   (= fd fill-pointer))
                              "~A fill pointer ~2,'0X:~8,'0X, expected ~D"
                              name ft fd fill-pointer)))
              (loop for lv in leader-list
                    for i from 0
                    when lv
                      do (multiple-value-bind (lt ld) (cw-ref w (- data 1 i))
                           (cold-check (and (= (tag-type lt) fixnum)
                                            (= ld lv))
                                       "~A leader ~D = ~2,'0X:~8,'0X, ~
expected ~D" name i lt ld lv)))
              ;; Data: spec-exact.
              (let* ((code (cold-array-type-code w type))
                     (packing (ldb (byte 3 1) code))
                     (len (if (listp dims) (reduce #'* dims) dims))
                     (ndims (if (listp dims) (length dims) 1))
                     (longp (or (> ndims 1) (>= len (ash 1 15))))
                     (base (+ data 1 (if longp (+ 3 (* 2 ndims)) 0)))
                     (nwords (if (zerop packing)
                                 len
                                 (ceiling len (ash 1 packing)))))
                (cold-check
                 (cond
                   (words
                    (loop for word in words
                          for vma from base
                          always (= word (nth-value 1 (cw-ref w vma)))))
                   (contents
                    (and (loop for c in contents
                               for vma from base
                               always (multiple-value-bind (et ed)
                                          (cw-ref w vma)
                                        (and (= (tag-type et) fixnum)
                                             (= ed c))))
                         ;; Tail beyond the spec'd prefix stays NIL.
                         (loop for i from (length contents) below len
                               always (multiple-value-bind (et ed)
                                          (cw-ref w (+ base i))
                                        (and (= et ntag) (= ed ndata))))))
                   (fill-fixnum
                    (loop for i below len
                          always (multiple-value-bind (et ed)
                                     (cw-ref w (+ base i))
                                   (and (= (tag-type et) fixnum)
                                        (= ed fill-fixnum)))))
                   ((zerop packing)
                    (loop for i below len
                          for expect-tag = (if (and last-cdr-nil
                                                    (= i (1- len)))
                                               (logior #x40 ntag)
                                               ntag)
                          always (multiple-value-bind (et ed)
                                     (cw-ref w (+ base i))
                                   (and (= et expect-tag) (= ed ndata)))))
                   (t
                    (loop for i below nwords
                          always (zerop (nth-value 1 (cw-ref w (+ base i)))))))
                 "~A data matches its spec" name)))))))))

(defun check-disk-events (w reference)
  "M3h gate: the five system disk-event variables reference four all-NIL
18-Q events with the distribution's headers; the serial event is the
storage root; the root's element 0 is the DISK-EVENT named-structure
symbol."
  (let ((array (cold-dtp w "ARRAY"))
        (names '("*ROOT-DISK-EVENT*" "*STORAGE-ROOT-DISK-EVENT*"
                 "*STORAGE-SERIAL-DISK-EVENT*" "*STORAGE-PARALLEL-DISK-EVENT*"
                 "*STORAGE-BACKGROUND-DISK-EVENT*"))
        (events '()))
    (multiple-value-bind (ntag ndata) (cold-nil-q w)
      (dolist (name names)
        (multiple-value-bind (tag data boundp)
            (cold-symbol-value-q w (make-vsym "STORAGE" name))
          (cold-check (and boundp (= (tag-type tag) array)
                           (<= #xF8040000 data #xF804FFFF))
                      "~A is a bound wired array" name)
          (push data events)
          (multiple-value-bind (ht hd) (cw-ref w data)
            (declare (ignore ht))
            (multiple-value-bind (rt rd) (reference-symbol-value reference name)
              (multiple-value-bind (rht rhd)
                  (if (and rt (= (tag-type rt) array))
                      (world-q reference rd)
                      nil)
                (declare (ignore rht))
                (cold-check (eql hd rhd)
                            "~A header ~8,'0X vs ref ~@[~8,'0X~]"
                            name hd rhd))))
          ;; Generator-fresh fields are all NIL (the root's element 0,
          ;; checked below, is the exception).
          (cold-check
           (loop for i from (if (string= name "*ROOT-DISK-EVENT*") 2 1) to 18
                 always (multiple-value-bind (et ed) (cw-ref w (+ data i))
                          (and (= et ntag) (= ed ndata))))
           "~A fields all start NIL" name)))
      (setf events (nreverse events))
      (cold-check (= (second events) (third events))
                  "the serial disk event is the storage root")
      (cold-check (= 4 (length (remove-duplicates events)))
                  "four distinct disk events")
      (multiple-value-bind (st sd)
          (cold-symbol-ref w (make-vsym "STORAGE" "DISK-EVENT"))
        (multiple-value-bind (et ed) (cw-ref w (+ (first events) 1))
          (cold-check (and (= (tag-type et) (tag-type st)) (= ed sd))
                      "root disk event element 0 is STORAGE:DISK-EVENT"))))))

(defun check-allocator-tables (w)
  "M3h gate: *ZONE-LEVEL* mirrors this world's regions (every region's
zone byte = its level, unpopulated zones -1); the stack registry holds
the initial SG's binding and control stacks sorted by origin; the array
metadata maps every known type code to its name symbol; every cold
area's name variable holds its area number (ldata.lisp:152-153
generator contract; dist-verified for all 22, M3h boot-11)."
  (let ((fixnum (cold-dtp w "FIXNUM")))
    ;; Area-name variables = area numbers.
    (let ((bad 0))
      (dotimes (i +cold-area-count+)
        (let* ((full-name (cold-area-name (cold-area w i)))
               (colon (position #\: full-name)))
          (multiple-value-bind (tag data boundp)
              (cold-symbol-value-q
               w (make-vsym (subseq full-name 0 colon)
                            (subseq full-name (1+ colon))))
            (unless (and boundp (= (tag-type tag) fixnum) (= data i))
              (incf bad)))))
      (cold-check (zerop bad)
                  "~D area-name variable~:P off their area numbers" bad))
    ;; *ZONE-LEVEL* vs the region table.
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS" "*ZONE-LEVEL*"))
      (declare (ignore tag))
      (cold-check boundp "*ZONE-LEVEL* is bound")
      (when boundp
        (let ((bytes (make-array 32 :initial-element #xFF)))
          (dotimes (word 8)
            (let ((v (nth-value 1 (cw-ref w (+ data 1 word)))))
              (dotimes (b 4)
                (setf (aref bytes (+ (* word 4) b))
                      (ldb (byte 8 (* 8 b)) v)))))
          (cold-check
           (loop for region across (cold-world-regions w)
                 always (= (aref bytes (ldb (byte 5 27)
                                            (cold-region-origin region)))
                           (ldb (byte 6 18) (cold-region-bits-for w region))))
           "every region's zone byte is its level"))))
    ;; Stack registry: two stacks, sorted, initial SG.
    (multiple-value-bind (tag n boundp)
        (cold-symbol-value-q
         w (make-vsym "SYSTEM-INTERNALS" "*NUMBER-OF-ACTIVE-STACKS*"))
      (cold-check (and boundp (= (tag-type tag) fixnum) (= n 2))
                  "*NUMBER-OF-ACTIVE-STACKS* = 2 (~@[~D~])" (and boundp n)))
    (let ((org (nth-value 1 (cold-symbol-value-q
                             w (make-vsym "SYSTEM-INTERNALS"
                                          "*STACK-ORIGIN*"))))
          (sg (cold-machinery w :initial-stack-group)))
      (cold-check
       (and org
            (< (nth-value 1 (cw-ref w (+ org 1)))
               (nth-value 1 (cw-ref w (+ org 2)))))
       "*STACK-ORIGIN* is sorted ascending")
      (let ((ssg (nth-value 1 (cold-symbol-value-q
                               w (make-vsym "SYSTEM-INTERNALS"
                                            "*STACK-STACK-GROUP*")))))
        (cold-check
         (and ssg
              (= (nth-value 1 (cw-ref w (+ ssg 1))) sg)
              (= (nth-value 1 (cw-ref w (+ ssg 2))) sg))
         "*STACK-STACK-GROUP* entries reference the initial SG")))
    ;; Array metadata: every known code maps to its named symbol.
    (let ((types (nth-value 1 (cold-symbol-value-q
                               w (make-vsym "SYSTEM" "*ARRAY-TYPES*")))))
      (cold-check
       (and types
            (loop for (name . code) in *cold-array-type-codes*
                  always (multiple-value-bind (st sd)
                             (cold-symbol-ref w (make-vsym "SYSTEM" name))
                           (multiple-value-bind (et ed)
                               (cw-ref w (+ types 1 code))
                             (and (= (tag-type et) (tag-type st))
                                  (= ed sd))))))
       "*ARRAY-TYPES* maps every known code to its symbol"))
    ;; *VALID-ARRAY-TYPE-CODES*: ART-BOOLEAN bitmap, one bit per defined
    ;; type code (icons.lisp:1597).  Two packed data words = the low/high
    ;; halves of the *cold-array-type-codes* bitmap.
    (let ((vatc (nth-value 1 (cold-symbol-value-q
                              w (make-vsym "SYSTEM" "*VALID-ARRAY-TYPE-CODES*"))))
          (bits (reduce (lambda (v pair) (logior v (ash 1 (cdr pair))))
                        *cold-array-type-codes* :initial-value 0)))
      (cold-check
       (and vatc
            (= (nth-value 1 (cw-ref w (+ vatc 1))) (ldb (byte 32 0) bits))
            (= (nth-value 1 (cw-ref w (+ vatc 2))) (ldb (byte 32 32) bits)))
       "*VALID-ARRAY-TYPE-CODES* marks every defined array type code"))))

(defun check-area-list (w)
  "M3h gate: SI:AREA-LIST is the cdr-coded area-name table data
(ldata.lisp:201 generator contract) and the last live area's Q is
cdr-nil so the list ends at the area count."
  (let ((name-tbl (cold-machinery w :area-name)))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS" "AREA-LIST"))
      (cold-check (and boundp (= (tag-type tag) (cold-dtp w "LIST"))
                       (= data (+ name-tbl 1)))
                  "AREA-LIST is the area-name table data (~@[~2,'0X:~8,'0X~])"
                  tag data))
    (multiple-value-bind (tag data)
        (cw-ref w (+ name-tbl +cold-area-count+))
      (declare (ignore data))
      (cold-check (= (ldb (byte 2 6) tag) 1)
                  "last live area Q is cdr-nil (~2,'0X)" tag))
    (multiple-value-bind (tag data)
        (cw-ref w (+ name-tbl +cold-area-count+ -1))
      (declare (ignore data))
      (cold-check (= (ldb (byte 2 6) tag) 0)
                  "penultimate area Q is cdr-next (~2,'0X)" tag))))

(defun cold-boot-init-forms (w)
  "Symbol vma -> raw init-form Q (tag . data) from the world's
SI:*COLD-LOAD-VARIABLE-INITIALIZATIONS*.  LISP-INITIALIZE-FIRST-TIME
does (SET VAR VAL) with VAL *unevaluated* for every listed variable
still unbound at first boot (sys/cold-load.lisp:527-528), BEFORE the
symbol-cell link pass -- so for the link invariant, an unbound value
cell whose symbol has an init row is effectively bound to the raw form
\(M3h boot 22: DEFAULT-CONS-AREA got the symbol WORKING-STORAGE-AREA)."
  (let ((map (make-hash-table))
        (dtp-list (cold-dtp w "LIST"))
        (dtp-symbol (cold-dtp w "SYMBOL")))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q
         w (make-vsym "SYSTEM-INTERNALS"
                      "*COLD-LOAD-VARIABLE-INITIALIZATIONS*"))
      (when boundp
        (cold-map-list
         w tag data
         (lambda (et ed evma)
           (declare (ignore evma))
           (when (= (tag-type et) dtp-list)
             (let ((row nil))
               (cold-map-list w et ed
                              (lambda (it id ivma)
                                (declare (ignore ivma))
                                (push (cons it id) row)
                                nil))
               (setf row (nreverse row))
               (when (and (= (length row) 2)
                          (= (tag-type (car (first row))) dtp-symbol))
                 (setf (gethash (cdr (first row)) map) (second row)))))
           nil))))
    map))

(defun check-bootstrap-link-invariant (w)
  "M3h boot 21 gate: BOOTSTRAP-LINK-SYMBOL-CELLS walks the
*LINKED-SYMBOL-CELLS* records at first boot and FERRORs \"Can't link two
cells with different values.\" when a record's two cells are both bound
with non-EQ contents (sys2/memory-cold.lisp:421-426).  Every record's
pair -- value cells for :VARIABLE, function cells for :FUNCTION -- must
reach the link pass with at most one bound side, or two EQ ones (boot 21:
LDATA's eager DEFVAR *READTABLE* stamp vs rdtbl's SETQ READTABLE).
Boot 22 extension: the invariant holds at BOOT, after the init loop has
raw-SET every still-unbound listed variable, so an unbound value cell
with an init row counts as bound to that raw form.  (The FSET stub loop
at cold-load.lisp:530 is the function-cell analog, but no stub name is a
link record, so function cells compare as built.)"
  (let ((bad nil)
        (n 0)
        (dtp-null (cold-dtp w "NULL"))
        (inits (cold-boot-init-forms w)))
    (dolist (rec (cold-world-linked-cells w))
      (destructuring-bind (from to type) rec
        (incf n)
        (let* ((funp (string= (vsym-name type) "FUNCTION"))
               (off (if funp 2 1)))
          (flet ((effective-q (vsym)
                   (multiple-value-bind (tag data)
                       (cw-ref w (cold-follow-cell
                                  w (+ off (cold-vsym w vsym))))
                     (let ((init (and (= (tag-type tag) dtp-null)
                                      (not funp)
                                      (gethash (cold-vsym w vsym) inits))))
                       (if init
                           (values (car init) (cdr init))
                           (values tag data))))))
            (multiple-value-bind (ft fd) (effective-q from)
              (multiple-value-bind (tt td) (effective-q to)
                (when (and (/= (tag-type ft) dtp-null)
                           (/= (tag-type tt) dtp-null)
                           (not (cold-q-eq ft fd tt td)))
                  (push (format nil "~A ~A - ~A (~2,'0X:~8,'0X vs ~2,'0X:~8,'0X)"
                                (vsym-name type) (vsym-name from)
                                (vsym-name to) ft fd tt td)
                        bad))))))))
    (cold-check (null bad)
                "bootstrap link invariant over ~D record~:P~@[; conflict: ~A~]"
                n (first bad))))

(defun cold-extent-size (w vma)
  "(values size kind) of the object at VMA under %FIND-STRUCTURE-EXTENT's
IMach rules (sys/objects.lisp:123-342), or (values nil reason) when the
Q there is not a recognizable object header."
  (multiple-value-bind (tag data) (cw-ref w vma)
    (let ((type (tag-type tag))
          (htype (ldb (byte 2 6) tag)))
      (flet ((array-size (hdr-data hdr-vma)
               ;; Qs from the header on: leader excluded.
               (let ((epq (ash 1 (ldb (byte 3 27) hdr-data))))
                 (if (logbitp 23 hdr-data)      ; long prefix
                     (multiple-value-bind (lt llen) (cw-ref w (1+ hdr-vma))
                       (declare (ignore lt))
                       (+ 4 (* 2 (ldb (byte 3 0) hdr-data))
                          (if (logbitp 14 hdr-data) 0  ; displaced
                              (ceiling llen epq))))
                     (+ 1 (ceiling (ldb (byte 15 0) hdr-data) epq))))))
        (cond
          ((= type (cold-dtp w "HEADER-P"))
           (case htype
             (0 (values 5 :symbol))
             (1 (multiple-value-bind (st sd) (cw-ref w (1- data))
                  (declare (ignore st))          ; instance: size Q sits
                  (values sd :instance)))        ; before the flavor
             (2 (multiple-value-bind (at ad) (cw-ref w data)
                  (if (and (= (tag-type at) (cold-dtp w "HEADER-I"))
                           (= (ldb (byte 2 6) at) 1))
                      (values (+ (- data vma) (array-size ad data))
                              :leader-array)
                      (values nil :leader-without-array))))
             (t (values nil :header-p-type-3))))
          ((= type (cold-dtp w "HEADER-I"))
           (case htype
             (0 (values (ldb (byte 18 0) data) :compiled-function))
             (1 (values (array-size data vma) :array))
             (2 (values (+ 1 (ldb (byte 27 0) data)) :bignum))
             (t (values nil :header-i-type-3))))
          (t (values nil :not-a-header)))))))

(defun check-boot-object-walk (w)
  "M3h boot 24 gate: BOOTSTRAP-FORWARD-SYMBOL-CELLS object-walks
SYMBOL-AREA and SAFEGUARDED-OBJECTS-AREA (pass 0) then
WIRED-CONTROL-TABLES, SAFEGUARDED-OBJECTS-AREA and COMPILED-FUNCTION-AREA
\(MAP-COMPILED-FUNCTIONS passes 1-2), applying %FIND-STRUCTURE-EXTENT at
every object boundary from the region origin (wired: from NIL,
objects.lisp:661-665) to the free pointer.  Any Q run the extent parser
cannot parse -- an unwritten gap, a headerless code block -- kills the
first boot with \"Found non-enclosing structure\", whose reporting path
is itself the unbound TRANSPORT-ERROR-ADDITIONAL-INFO trap.

M3h boot 25 extension: pass 1 also calls COMPILED-FUNCTION-NAME on
every compiled function it walks, which reads CAR of the Q at
cca + (CCA-TOTAL-SIZE - CCA-SUFFIX-SIZE) (sys2/macro.lisp
CCA-EXTRA-INFO #+IMACH).  Suffix 0 sends that read one Q past the
block (boot 25 trapped on the catch-all's neighbor), and a non-list
extra-info Q either traps or FERRORs in CAR.  Every CCA must have
suffix >= 1, its extra-info inside the block past the self-pointer,
and an extra-info Q of type DTP-LIST or DTP-NIL (survey of the built
world: all 5,849 real CCAs are DTP-LIST)."
  (let ((bad nil)
        (objects 0)
        (dtp-list (cold-dtp w "LIST"))
        (dtp-nil (cold-dtp w "NIL")))
    (dolist (area-name '("SYMBOL-AREA" "SAFEGUARDED-OBJECTS-AREA"
                         "WIRED-CONTROL-TABLES" "COMPILED-FUNCTION-AREA"))
      (let ((area (cold-area w area-name)))
        (dolist (rn (cold-area-regions area))
          (let* ((region (aref (cold-world-regions w) rn))
                 (vma (if (string= area-name "WIRED-CONTROL-TABLES")
                          (cold-world-nil-vma w)
                          (cold-region-origin region)))
                 (limit (cold-region-free region)))
            (loop while (< vma limit)
                  do (multiple-value-bind (size kind) (cold-extent-size w vma)
                       (cond ((null size)
                              (multiple-value-bind (tag data) (cw-ref w vma)
                                (push (format nil "~A: ~A at ~8,'0X ~
(Q ~2,'0X:~8,'0X)" area-name kind vma tag data)
                                      bad))
                              (return))
                             ((or (<= size 0) (> size (- limit vma)))
                              (push (format nil "~A: ~A at ~8,'0X has size ~
~D (region ends at ~8,'0X)" area-name kind vma size limit)
                                    bad)
                              (return))
                             (t (when (eq kind :compiled-function)
                                  (multiple-value-bind (ht hd) (cw-ref w vma)
                                    (declare (ignore ht))
                                    (let* ((suffix (ldb (byte 14 18) hd))
                                           (xinfo (+ vma (- size suffix))))
                                      (if (or (< suffix 1)
                                              (< (- size suffix) 2))
                                          (push (format nil "~A: CCA at ~
~8,'0X extra-info outside block (suffix ~D total ~D)"
                                                        area-name vma suffix
                                                        size)
                                                bad)
                                          (multiple-value-bind (xt xd)
                                              (cw-ref w xinfo)
                                            (declare (ignore xd))
                                            (unless (member (tag-type xt)
                                                            (list dtp-list
                                                                  dtp-nil))
                                              (push (format nil "~A: CCA at ~
~8,'0X extra-info Q ~8,'0X has type #x~2,'0X, not list or NIL"
                                                            area-name vma
                                                            xinfo
                                                            (tag-type xt))
                                                    bad)))))))
                                (incf objects)
                                (incf vma size)))))))))
    (cold-check (null bad)
                "boot object walk parses ~D object~:P~@[; first: ~A~]"
                objects (first bad))))

;;; (cold-symbol-pname-at / cold-get-property-q live in cold-object.lisp:
;;; cold-do-fdefine's derived-fspec stamp reads the same properties.)

(defun cold-list-second-q (w list-vma)
  "(values tag data) of the second element Q of the cdr-coded list at
LIST-VMA, or NIL when the list has no second element."
  (multiple-value-bind (t0 d0) (cw-ref w list-vma)
    (declare (ignore d0))
    (case (ldb (byte 2 6) t0)
      (0 (cw-ref w (1+ list-vma)))          ; cdr-next: element follows
      (1 (values nil nil))                  ; cdr-nil: single element
      (t (multiple-value-bind (ct cd)       ; cdr-normal: follow the cdr Q
             (cw-ref w (1+ list-vma))
           (if (= (tag-type ct) (cold-dtp w "LIST"))
               (cw-ref w cd)
               (values nil nil)))))))

(defun check-pass1-fspec-handlers (w)
  "M3h boot 26/27 gate: pass 1 of BOOTSTRAP-FORWARD-SYMBOL-CELLS calls
FDEFINEDP on every walked CCA's extra-info name.  For a LIST name,
VALIDATE-FUNCTION-SPEC requires (GET head 'SYS:FUNCTION-SPEC-HANDLER)
to be a FUNCALLable handler -- the failure arm DBG:CHECK-ARG-1 is
warm-only, so a single list-named CCA whose head fails validation
kills the first boot (boot 26: (FLAVOR:METHOD :TYO DRIBBLE-STREAM)
before the flavor runtime joined the cold set; boot 27:
\(GLOBAL:SETF BYTE-SWAPPED-LOCATIVE-REF-32) -- the resolver had homed
the head on the one SETF without the derived-type DEFPROPs).  Replays
that read: every list-fspec head among all walked CCA names must
carry a FUNCTION-SPEC-HANDLER property whose value symbol has a
compiled function in its function cell.  When the handler is
DERIVED-FUNCTION-SPEC-HANDLER (fspec.lisp:1583), validation further
requires the head's DERIVED-FUNCTION-PROPERTY, and pass 1's FDEFINEDP
then reads (GET derived-from <that property>) -- which must be a
locative to a cell holding a compiled function (the cold-do-fdefine
derived stamp), or the definition silently vanishes at boot."
  (let ((bad nil)
        (list-named 0)
        (head-cache (make-hash-table))
        (heads nil)
        (dtp-list (cold-dtp w "LIST"))
        (dtp-symbol (cold-dtp w "SYMBOL"))
        (dtp-locative (cold-dtp w "LOCATIVE"))
        (dtp-cf (cold-dtp w "COMPILED-FUNCTION")))
    (flet ((head-info (head-vma)
             "(problem . derived-ind-vma): problem NIL when the head
validates; derived-ind-vma = the DERIVED-FUNCTION-PROPERTY indicator
symbol when the head is a derived function-spec type."
             (multiple-value-bind (cached foundp) (gethash head-vma head-cache)
               (when foundp (return-from head-info cached)))
             (setf (gethash head-vma head-cache)
                   (multiple-value-bind (vt vd foundp)
                       (cold-get-property-q w head-vma
                                            "FUNCTION-SPEC-HANDLER")
                     (cond
                       ((not foundp)
                        (cons "no FUNCTION-SPEC-HANDLER property" nil))
                       ;; DEFINE-FUNCTION-SPEC-HANDLER stores the
                       ;; compiled function itself ((:PROPERTY head
                       ;; FUNCTION-SPEC-HANDLER) defun);
                       ;; DEFINE-DERIVED-FUNCTION-TYPE and the flavor
                       ;; runtime store a symbol to FUNCALL through.
                       ((= (tag-type vt) dtp-cf) (cons nil nil))
                       ((/= (tag-type vt) dtp-symbol)
                        (cons (format nil "handler Q ~2,'0X:~8,'0X neither ~
symbol nor compiled function" vt vd)
                              nil))
                       (t
                        (multiple-value-bind (ft fd)
                            (cw-ref w (cold-follow-cell w (+ vd 2)))
                          (declare (ignore fd))
                          (cond
                            ((/= (tag-type ft) dtp-cf)
                             (cons (format nil "handler ~A function cell ~
unbound (tag ~2,'0X)"
                                           (cold-symbol-pname-at w vd)
                                           ft)
                                   nil))
                            ((equal (cold-symbol-pname-at w vd)
                                    "DERIVED-FUNCTION-SPEC-HANDLER")
                             (multiple-value-bind (dt dd dfound)
                                 (cold-get-property-q
                                  w head-vma "DERIVED-FUNCTION-PROPERTY")
                               (if (and dfound (= (tag-type dt) dtp-symbol))
                                   (cons nil dd)
                                   (cons (concatenate 'string
                                                      "derived head lacks a "
                                                      "symbol-valued DERIVED-"
                                                      "FUNCTION-PROPERTY "
                                                      "property")
                                         nil))))
                            (t (cons nil nil)))))))))
           (derived-problem (ind-vma name-list-vma)
             "NIL when (GET derived-from <ind>) is a locative to a cell
holding a compiled function; else a reason string."
             (multiple-value-bind (st sd)
                 (cold-list-second-q w name-list-vma)
               (cond
                 ((or (null st) (/= (tag-type st) dtp-symbol))
                  "second element is not a symbol")
                 (t
                  (let ((ind-pname (cold-symbol-pname-at w ind-vma)))
                    (multiple-value-bind (pt pd pfound)
                        (cold-get-property-q w sd ind-pname)
                      (cond
                        ((not pfound)
                         (format nil "~A lacks the ~A property"
                                 (cold-symbol-pname-at w sd) ind-pname))
                        ((/= (tag-type pt) dtp-locative)
                         (format nil "~A property of ~A is ~
~2,'0X:~8,'0X, not a locative"
                                 ind-pname (cold-symbol-pname-at w sd)
                                 pt pd))
                        (t
                         (multiple-value-bind (ct cd)
                             (cw-ref w (cold-follow-cell w pd))
                           (declare (ignore cd))
                           (unless (= (tag-type ct) dtp-cf)
                             (format nil "~A derived cell unbound ~
(tag ~2,'0X)"
                                     (cold-symbol-pname-at w sd)
                                     ct))))))))))))
      (dolist (area-name '("SYMBOL-AREA" "SAFEGUARDED-OBJECTS-AREA"
                           "WIRED-CONTROL-TABLES" "COMPILED-FUNCTION-AREA"))
        (let ((area (cold-area w area-name)))
          (dolist (rn (cold-area-regions area))
            (let* ((region (aref (cold-world-regions w) rn))
                   (vma (if (string= area-name "WIRED-CONTROL-TABLES")
                            (cold-world-nil-vma w)
                            (cold-region-origin region)))
                   (limit (cold-region-free region)))
              (loop while (< vma limit)
                    do (multiple-value-bind (size kind)
                           (cold-extent-size w vma)
                         (unless (and size (plusp size)) (return))
                         (when (eq kind :compiled-function)
                           (multiple-value-bind (ht hd) (cw-ref w vma)
                             (declare (ignore ht))
                             (let ((xinfo (+ vma (- size
                                                    (ldb (byte 14 18) hd)))))
                               (multiple-value-bind (xt xd) (cw-ref w xinfo)
                                 (when (= (tag-type xt) dtp-list)
                                   ;; extra-info list; car = the name
                                   (multiple-value-bind (nt nd) (cw-ref w xd)
                                     (when (= (tag-type nt) dtp-list)
                                       (incf list-named)
                                       (multiple-value-bind (et ed)
                                           (cw-ref w nd)
                                         (if (/= (tag-type et) dtp-symbol)
                                             (push (format nil "CCA ~8,'0X: ~
list fspec head is not a symbol (~2,'0X:~8,'0X)" vma et ed)
                                                   bad)
                                             (destructuring-bind
                                                 (problem . derived-ind)
                                                 (head-info ed)
                                               (let ((pname
                                                       (cold-symbol-pname-at
                                                        w ed)))
                                                 (when (and problem
                                                            (not (member
                                                                  pname heads
                                                                  :test
                                                                  #'equal)))
                                                   (push (format nil "CCA ~
~8,'0X (head ~A): ~A" vma pname problem)
                                                         bad))
                                                 (pushnew pname heads
                                                          :test #'equal)
                                                 ;; Derived spec: FDEFINEDP
                                                 ;; reads (GET derived-from
                                                 ;; <indicator>) -- must be
                                                 ;; a locative to a cell
                                                 ;; holding a compiled
                                                 ;; function.
                                                 (when derived-ind
                                                   (let ((p (derived-problem
                                                             derived-ind nd)))
                                                     (when p
                                                       (push (format nil
                                                                     "CCA ~
~8,'0X (~A spec): ~A" vma pname p)
                                                             bad)))))))))))))))
                         (incf vma size))))))))
    (cold-check (null bad)
                "pass-1 fspec handlers: ~D list-named CCA~:P, heads ~
{~{~A~^ ~}}~@[; problems: ~{~A~^ | ~}~]"
                list-named heads (reverse bad))))

(defun check-plist-termination (w)
  "M3h boot 23 gate: every interned symbol's plist must be a
NIL-terminated cdr-coded chain.  cold-prepend-property builds
\(ind val . next) triples whose value Q carries cdr-normal; any raw
cw-set into a property cell zeroes that code and splices the rest of
PROPERTY-LIST-AREA into the walk, so GET runs off the allocation
frontier and traps on the first unwritten Q (boot 23: the
FUNCTION-CELL-STORAGE-CATEGORY miss on CL:< from the first :FUNCTION
link record).  Symbols on link records must also be NULL-free:
BOOTSTRAP-LINK-SYMBOL-CELLS GETs their full plists pre-banner and a
DTP-NULL car is the same trap 71."
  (let ((bad nil)
        (nsyms 0)
        (nqs 0)
        (null-cells 0)
        (dtp-null (cold-dtp w "NULL"))
        (dtp-list (cold-dtp w "LIST"))
        (linked (make-hash-table)))
    (dolist (rec (cold-world-linked-cells w))
      (setf (gethash (cold-vsym w (first rec)) linked) t
            (gethash (cold-vsym w (second rec)) linked) t))
    (maphash
     (lambda (key sym-vma)
       (incf nsyms)
       (multiple-value-bind (tag data) (cw-ref w (+ sym-vma 3))
         (let ((steps 0))
           (loop
             (when (cold-q-nil-p w tag data) (return))
             (unless (= (tag-type tag) dtp-list)
               (push (format nil "~A: plist cdr is ~2,'0X:~8,'0X"
                             (car key) tag data)
                     bad)
               (return))
             (when (> (incf steps) 4096)
               (push (format nil "~A: plist walk did not terminate"
                             (car key))
                     bad)
               (return))
             (let ((vma (cold-follow-cell w data)))
               (multiple-value-bind (ct cd) (cw-ref w vma)
                 (when (= (tag-type ct) dtp-null)
                   (incf null-cells)
                   (when (gethash sym-vma linked)
                     (push (format nil "~A: NULL Q at ~8,'0X on a ~
link-record symbol" (car key) vma)
                           bad))
                   ;; an unbound cell's walk continues normally: the
                   ;; NULL Q keeps the cdr code the pair was built with
                   )
                 (incf nqs)
                 (ecase (ldb (byte 2 6) ct)
                   (0 (setf tag (tag 0 dtp-list) data (1+ vma)))
                   (1 (return))
                   ((2 3) (multiple-value-setq (tag data)
                            (cw-ref w (cold-follow-cell w (1+ vma))))))))))))
     (cold-world-symbols w))
    (cold-check (null bad)
                "plists of ~D symbol~:P terminate (~D Qs walked, ~
~D unbound cell~:P)~@[; first: ~A~]"
                nsyms nqs null-cells (first bad))))

(defun check-linked-symbol-cells (w)
  "M3h gate: SI:*LINKED-SYMBOL-CELLS* carries the (from to type) records
that permanent-links' SI:LINK-SYMBOL-*-CELLS load forms accumulated, in
load order, for BOOTSTRAP-FORWARD-SYMBOL-CELLS to consume at first boot
(sys2/memory-cold.lisp:286-292).  The dist's NIL is the consumed state
and is only correct while no link form has been loaded."
  (let ((records (reverse (cold-world-linked-cells w))))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS"
                                          "*LINKED-SYMBOL-CELLS*"))
      (if (null records)
          (cold-check (and boundp (cold-q-nil-p w tag data))
                      "*LINKED-SYMBOL-CELLS* is NIL (no links recorded)")
          (let ((dtp-list (cold-dtp w "LIST"))
                (n 0)
                (bad 0)
                (selfs nil))
            (cold-check (and boundp (= (tag-type tag) dtp-list))
                        "*LINKED-SYMBOL-CELLS* is a list (~2,'0X:~8,'0X)"
                        tag data)
            (when (and boundp (= (tag-type tag) dtp-list))
              (cold-map-list
               w tag data
               (lambda (et ed evma)
                 (declare (ignore evma))
                 (let ((rec (pop records)))
                   (incf n)
                   (if (and rec (= (tag-type et) dtp-list))
                       (destructuring-bind (from to kind) rec
                         (let ((want (list (cold-vsym w from)
                                           (cold-vsym w to)
                                           (cold-vsym w kind)))
                               (got nil))
                           ;; A collapsed FROM/TO pair (same-pname symbols
                           ;; coalesced across packages, e.g. CL:/ and
                           ;; ZL:/) would make BOOTSTRAP-FORWARD-SYMBOL-
                           ;; CELLS forward a cell onto itself at boot.
                           (when (= (first want) (second want))
                             (push (list (vsym-package from) (vsym-name from)
                                         (vsym-package to) (vsym-name to))
                                   selfs))
                           (cold-map-list
                            w (tag 0 dtp-list) ed
                            (lambda (st sd svma)
                              (declare (ignore svma))
                              (push (list (tag-type st) sd) got)
                              nil))
                           (unless (equal (mapcar #'second (reverse got))
                                          want)
                             (incf bad))))
                       (incf bad))
                   nil))))
            (cold-check (null selfs)
                        "no self-links among the linked-cell records ~
(~D found: ~S)" (length selfs) selfs)
            (cold-check (and (zerop bad) (null records))
                        "~D linked-cell record~:P mirror the load pass ~
(~D bad, ~D missing)"
                        n bad (length records)))))))

(defun check-keyword-self-eval (w)
  "M3h gate: every interned KEYWORD-package symbol self-evaluates -- its
value cell is a one-q-forward into the generator's :SELF-EVALUATING table
(CONSTANTS-AREA) whose slot holds the symbol back.  BUILD-INITIAL-PACKAGES
EVALs the DEFPACKAGE-INTERNAL forms (package.lisp:2393) before
BOOTSTRAP-FORWARD-SYMBOL-CELLS, so keywords must already be forwarded in
the cold image (cf. PKG-NEW-KEYWORD-SYMBOL, package.lisp:1125)."
  (let ((tbl (cold-world-self-eval-table w))
        (fill (cold-world-self-eval-fill w))
        (fwd (cold-dtp w "ONE-Q-FORWARD"))
        (sym-dtp (cold-dtp w "SYMBOL"))
        (nkw 0) (bad 0))
    (cold-check (plusp tbl)
                "self-evaluating symbol table was created (header #x~8,'0X)"
                tbl)
    (maphash
     (lambda (key vma)
       (when (equal (cdr key) "KEYWORD")
         (incf nkw)
         (multiple-value-bind (vt vd) (cw-ref w (+ vma 1))
           (let ((slotp (and (= (tag-type vt) fwd)
                             (>= vd (+ tbl 1))
                             (< vd (+ tbl 1 fill)))))
             (unless (and slotp
                          (multiple-value-bind (st sd) (cw-ref w vd)
                            (and (= (tag-type st) sym-dtp) (= sd vma))))
               (incf bad))))))
     (cold-world-symbols w))
    (cold-check (= nkw fill)
                "~D keyword~:P forwarded, table fill pointer ~D" nkw fill)
    (cold-check (zerop bad)
                "~D keyword value cell~:P self-evaluate (~D not forwarded ~
to a slot naming the symbol)" nkw bad)))

(defun check-relative-name-deferral (w)
  "M3h boot-14 gate: no DEFPACKAGE-INTERNAL call in the stored
BUILD-INITIAL-PACKAGES list carries :RELATIVE-NAMES (MAKE-PACKAGE's
handling COPYTREEs a dotted (name . package) alist; COPYLIST's LENGTH
ENDPs the package object and the trap is unresumable pre-banner), and
the withheld triples ride *COLD-LOAD-DEFERRED-FORMS* as
SI:PKG-ADD-RELATIVE-NAME calls instead (the RE-MAKE-PACKAGE path,
package.lisp:1393).  8.5 PKGDCL has exactly two live clauses:
I-LISP-COMPILER (I GLOBAL) and NETBOOT (WT WORLD-TOOLS)."
  (let* ((dtp-list (cold-dtp w "LIST"))
         (dtp-symbol (cold-dtp w "SYMBOL"))
         (rel-kw (gethash (cons "RELATIVE-NAMES"
                                (cold-resolve-home "RELATIVE-NAMES"
                                                   "KEYWORD"))
                          (cold-world-symbols w)))
         (triples (cold-world-relative-names w))
         (stray 0))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS"
                                          "BUILD-INITIAL-PACKAGES"))
      (when (and boundp (= (tag-type tag) dtp-list) rel-kw)
        (cold-map-list
         w tag data
         (lambda (ct cd cvma)
           (declare (ignore cvma))
           (when (= (tag-type ct) dtp-list)
             (cold-map-list
              w (tag 0 dtp-list) cd
              (lambda (et ed evma)
                (declare (ignore evma))
                (when (and (= (tag-type et) dtp-symbol) (= ed rel-kw))
                  (incf stray))
                nil)))
           nil))))
    (cold-check (zerop stray)
                ":RELATIVE-NAMES withheld from BUILD-INITIAL-PACKAGES ~
(~D stray clause~:P)" stray)
    (cold-check (<= 2 (length triples) 6)
                "~D relative-name triple~:P withheld for deferral"
                (length triples))
    (let ((add-name (and triples
                         (cold-vsym w (make-vsym "SYSTEM-INTERNALS"
                                                 "PKG-ADD-RELATIVE-NAME"))))
          (found 0))
      (multiple-value-bind (tag data boundp)
          (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS"
                                            "*COLD-LOAD-DEFERRED-FORMS*"))
        (when (and boundp (= (tag-type tag) dtp-list) add-name)
          (cold-map-list
           w tag data
           (lambda (ct cd cvma)
             (declare (ignore cvma))
             (when (= (tag-type ct) dtp-list)
               (multiple-value-bind (ht hd)
                   (cw-ref w (cold-follow-cell w cd))
                 (when (and (= (tag-type ht) dtp-symbol) (= hd add-name))
                   (incf found))))
             nil))))
      (cold-check (= found (length triples))
                  "~D deferred SI:PKG-ADD-RELATIVE-NAME form~:P (~D ~
withheld)" found (length triples)))))

(defun check-split-symbol-resolution (w)
  "M3h boot-15 gate: Genera's dialect-split pnames stay split.  FORMAT is
the witness: GLOBAL exports its own (pkgdcl.lisp:6614) while SCL/FCL
import theirs from LISP (pkgdcl.lisp:3172, \"string incompatibility\"),
so CLCP iofns' wrapper (calls warm FORMAT:FORMAT-INTERNAL) must land on
LISP:FORMAT and leave GLOBAL:FORMAT unbound for the FBOUNDP-guarded
FORMAT-COLD-LOAD stub (cold-load.lisp:530,156).  aarray.lisp's
SORT-AARRAY-PROGRESS-NOTE FORMATs during BUILD-INITIAL-PACKAGES."
  (cold-check (string= (cold-resolve-home "FORMAT" "COMMON-LISP-INTERNALS")
                       "LISP")
              "CLI context resolves FORMAT to LISP (SCL imports it)")
  (cold-check (string= (cold-resolve-home "FORMAT" "SYSTEM-INTERNALS")
                       "GLOBAL")
              "SI context resolves FORMAT to GLOBAL")
  (let ((zl (gethash (cons "FORMAT" "GLOBAL") (cold-world-symbols w)))
        (cl (gethash (cons "FORMAT" "LISP") (cold-world-symbols w)))
        (dtp-null (cold-dtp w "NULL")))
    (cold-check (and zl cl (/= zl cl))
                "GLOBAL:FORMAT and LISP:FORMAT are distinct symbols")
    (when (and zl cl (/= zl cl))
      (multiple-value-bind (tag data) (cw-ref w (+ zl 2))
        (declare (ignore data))
        (cold-check (= (tag-type tag) dtp-null)
                    "GLOBAL:FORMAT function cell is unbound (boot stub ~
installs at FSET time)"))
      (multiple-value-bind (tag data) (cw-ref w (+ cl 2))
        (declare (ignore data))
        (cold-check (/= (tag-type tag) dtp-null)
                    "LISP:FORMAT function cell carries the CLCP ~
wrapper")))))

(defun check-nil-t-stamp (w)
  "M3h boot-16 gate: the architectural NIL/T blocks carry real pnames,
NIL plists, and \"LISP\" home-package strings (the skeleton's zero
placeholders trap in BUILD-INITIAL-PACKAGES's hand FIXUP-SYMBOL-PACKAGE
of T/NIL, package.lisp:2412; dist homes both in LISP)."
  (let ((dtp-header-p (cold-dtp w "HEADER-P"))
        (dtp-string (cold-dtp w "STRING")))
    (loop for (vma pname) in (list (list (cold-world-nil-vma w) "NIL")
                                   (list (cold-world-t-vma w) "T"))
          do (multiple-value-bind (tag data) (cw-ref w vma)
               (cold-check (and (= (tag-type tag) dtp-header-p)
                                (plusp data)
                                (equal (cold-read-string w data) pname))
                           "~A+0 pname is ~S" pname pname))
             (multiple-value-bind (tag data) (cw-ref w (+ vma 3))
               (cold-check (cold-q-nil-p w tag data)
                           "~A+3 plist is NIL" pname))
             (multiple-value-bind (tag data) (cw-ref w (+ vma 4))
               (cold-check (and (= (tag-type tag) dtp-string)
                                (plusp data)
                                (equal (cold-read-string w data) "LISP"))
                           "~A+4 package is the \"LISP\" name string"
                           pname)))))

(defun check-declared-package-homes (w)
  "M3h boot-17 gate: every cold symbol's home package is one PKGDCL
declares, i.e. one BUILD-INITIAL-PACKAGES will create before the
FIXUP-SYMBOL-PACKAGE sweep PKG-FIND-PACKAGEs each symbol's package-slot
string (package.lisp:2398).  An undeclared home (CLTL-INTERNALS) makes
that sweep SIGNAL PACKAGE-NOT-FOUND pre-banner."
  (let ((bad nil))
    (maphash (lambda (key vma)
               (declare (ignore vma))
               (let ((pkg (cdr key)))
                 (when (and pkg (not (cold-declared-package-p pkg)))
                   (pushnew pkg bad :test #'string=))))
             (cold-world-symbols w))
    (cold-check (null bad)
                "every symbol home is a PKGDCL-declared package ~
(undeclared: ~S)" bad)))

(defun check-export-home-conflicts (w)
  "M3h boot-19 gate: no cold symbol is homed in a package that inherits
\(pkgdcl :USE, transitively) from a package whose :EXPORT lists the same
pname -- BUILD-INITIAL-PACKAGES' import-export pass would EXPORT the
pname from the exporter, INTERN a fresh symbol there, and signal
NAME-CONFLICT-IN-EXPORT against the cold one.  The emitted symbol table
must be a fixed point of COLD-ADJUST-HOME-FOR-EXPORTS.  Witness:
CLASS-OF belongs to FUTURE-COMMON-LISP (pkgdcl.lisp:1849), never CLOS
\(the dist's warm home)."
  (let ((bad nil))
    (maphash (lambda (key vma)
               (declare (ignore vma))
               (destructuring-bind (pname . home) key
                 (when (and home
                            (not (string= (cold-adjust-home-for-exports
                                           pname home)
                                          home)))
                   (push key bad))))
             (cold-world-symbols w))
    (cold-check (null bad)
                "no symbol homed below an exporter of its pname ~
(~D found: ~S)" (length bad) (subseq bad 0 (min 5 (length bad)))))
  (cold-check (string= (cold-resolve-home "CLASS-OF" "CLOS-INTERNALS")
                       "FUTURE-COMMON-LISP")
              "CLASS-OF resolves to FUTURE-COMMON-LISP")
  (cold-check (null (gethash (cons "CLASS-OF" "CLOS")
                             (cold-world-symbols w)))
              "no cold symbol CLOS:CLASS-OF")
  ;; M3h boot 20: SCL :IMPORT-FROMs *PRINT-READABLY* out of
  ;; FUTURE-COMMON-LISP (pkgdcl.lisp:3620) -- a cold symbol homed in SCL
  ;; makes IMPORT-INTERNAL signal NAME-CONFLICT-IN-IMPORT.
  (cold-check (string= (cold-resolve-home "*PRINT-READABLY*"
                                          "SYMBOLICS-COMMON-LISP")
                       "FUTURE-COMMON-LISP")
              "*PRINT-READABLY* resolves to FUTURE-COMMON-LISP")
  (cold-check (null (gethash (cons "*PRINT-READABLY*"
                                   "SYMBOLICS-COMMON-LISP")
                             (cold-world-symbols w)))
              "no cold symbol SCL:*PRINT-READABLY*"))

(defun check-package-boot-simulation (w)
  "M3h boot-20 gate: symbolically run BUILD-INITIAL-PACKAGES
\(package.lisp:2358) over the emitted symbol table and the PKGDCL clause
set -- the FIXUP-SYMBOL-PACKAGE sweep in address order, the shadow pass,
the import-export pass in pkgdcl order, and the safeguarded-symbol
check.  Any would-be signal is fatal pre-banner (SIGNAL-COLD-LOAD prints
to unbound *TERMINAL-IO*, cold-load.lisp:345): NAME-CONFLICT-IN-EXPORT
from EXPORT-INTERNAL's used-by loop or PKG-NEW-SYMBOL's fresh-external
path (package.lisp:1473/1080), NAME-CONFLICT-IN-IMPORT from
IMPORT-INTERNAL (package.lisp:1515), PACKAGE-LOCKED from a fresh intern
into a locked (:EXTERNAL-ONLY) source outside its own WITH-PACKAGE-LOCK
\(package.lisp:1097), and the \"didn't get safeguarded\" ERROR
\(package.lisp:2437).  Symbol identity = (pname . home) cold keys;
fresh boot interns get (pname :FRESH pkg) identities.  Visibility is
CL:FIND-SYMBOL's: present, else the EXTERNALs of the DIRECT use list."
  (unless *cold-package-defs*
    (return-from check-package-boot-simulation))
  (let ((present (make-hash-table :test #'equal))  ; (pkg . pname) -> (id . code)
        (shadowsets (make-hash-table :test #'equal)) ; pkg -> id list
        (uses (make-hash-table :test #'equal))       ; pkg -> primary use list
        (users (make-hash-table :test #'equal))      ; pkg -> direct users
        (extonly *cold-package-external-only*)
        (fails nil))
    (dolist (def *cold-package-defs*)
      (setf (gethash (getf def :pkg) uses)
            (mapcar #'cold-package-primary
                    (gethash (getf def :pkg) *cold-package-uses*))))
    (dolist (def *cold-package-defs*)
      (dolist (u (gethash (getf def :pkg) uses))
        (push (getf def :pkg) (gethash u users))))
    (labels ((fail (fmt &rest args)
               (push (apply #'format nil fmt args) fails))
             (visible (pname pkg)
               (let ((own (gethash (cons pkg pname) present)))
                 (if own
                     (car own)
                     (loop for q in (gethash pkg uses)
                           for e = (gethash (cons q pname) present)
                           when (and e (eq (cdr e) :external))
                             return (car e)))))
             (shadowed-p (id pkg)
               (member id (gethash pkg shadowsets) :test #'equal))
             (add (pkg pname id code where)
               ;; PKG-NEW-SYMBOL: a symbol becoming external is checked
               ;; against everything visible in the direct users.
               (when (eq code :external)
                 (dolist (u (gethash pkg users))
                   (let ((v (visible pname u)))
                     (when (and v (not (equal v id))
                                (not (shadowed-p v u)))
                       (fail "~A: ~A:~A external conflicts with ~S in user ~A"
                             where pkg pname v u)))))
               (setf (gethash (cons pkg pname) present) (cons id code)))
             (new-code (pkg)
               (if (gethash pkg extonly) :external :internal))
             (cl-intern (pname pkg unlocked where)
               ;; CL:INTERN: visible symbol, else a fresh present one.
               (or (visible pname pkg)
                   (let ((id (list pname :fresh pkg)))
                     (when (and (gethash pkg extonly) (not unlocked))
                       (fail "~A: fresh intern of ~A into locked ~A ~
(PACKAGE-LOCKED signal)" where pname pkg))
                     (add pkg pname id (new-code pkg) where)
                     id)))
             (import-1 (s pname pkg where)
               ;; IMPORT-INTERNAL: conflict against anything visible.
               (let ((v (visible pname pkg)))
                 (cond ((null v) (add pkg pname s (new-code pkg) where))
                       ((not (equal v s))
                        (fail "~A: NAME-CONFLICT-IN-IMPORT of ~A into ~A ~
against ~S" where pname pkg v))
                       ((not (gethash (cons pkg pname) present))
                        (add pkg pname s (new-code pkg) where))))))
      ;; FIXUP-SYMBOL-PACKAGE sweep: T and NIL by hand (homed LISP), then
      ;; the cold symbols in address = interning order.
      (let ((syms nil))
        (maphash (lambda (key vma) (push (cons vma key) syms))
                 (cold-world-symbols w))
        (setf syms (sort syms #'< :key #'car))
        (dolist (nt '("T" "NIL"))
          (add "LISP" nt (cons nt "LISP") (new-code "LISP") "sweep"))
        (loop for (nil . key) in syms
              do (add (cdr key) (car key) key (new-code (cdr key))
                      "sweep")))
      ;; Shadow pass (PKG-BOOTSTRAP-SHADOW, MAKE-PACKAGE-SHADOW):
      ;; :SHADOWING-IMPORT, then :SHADOW inside :IMPORT-FROM = shadowing
      ;; import from the source, then plain :SHADOW.
      (dolist (def *cold-package-defs*)
        (let* ((pkg (getf def :pkg))
               (shadow (getf def :shadow)))
          (flet ((note-shadowing-import (s pname)
                   (setf (gethash (cons pkg pname) present)
                         (cons s :internal))
                   (pushnew s (gethash pkg shadowsets) :test #'equal)))
            (loop for (src . pname) in (getf def :shadowing-import)
                  do (note-shadowing-import
                      (cl-intern pname (cold-package-primary src) nil
                                 "shadow pass")
                      pname))
            (dolist (grp (getf def :import-from))
              (destructuring-bind (src . names) grp
                (dolist (n names)
                  (when (member n shadow :test #'string=)
                    (note-shadowing-import
                     (cl-intern n (cold-package-primary src) nil
                                "shadow pass")
                     n)))))
            (dolist (n shadow)
              (let ((own (gethash (cons pkg n) present)))
                (unless own
                  (setf own (cons (list n :fresh pkg) :internal)
                        (gethash (cons pkg n) present) own))
                (pushnew (car own) (gethash pkg shadowsets)
                         :test #'equal))))))
      ;; Import-export pass (PKG-BOOTSTRAP-IMPORT-EXPORT), pkgdcl order;
      ;; each package unlocked for its own clauses (WITH-PACKAGE-LOCK).
      (dolist (def *cold-package-defs*)
        (let ((pkg (getf def :pkg)))
          (loop for (src . pname) in (getf def :import)
                do (import-1 (cl-intern pname (cold-package-primary src)
                                        nil "import")
                             pname pkg "import"))
          (dolist (grp (getf def :import-from))
            (destructuring-bind (src . names) grp
              (let ((srcp (cold-package-primary src)))
                (dolist (n names)
                  (import-1 (cl-intern n srcp (string= srcp pkg)
                                       "import-from")
                            n pkg "import-from")))))
          (dolist (n (getf def :export))
            (let ((s (cl-intern n pkg t "export")))
              (dolist (u (gethash pkg users))
                (let ((v (visible n u)))
                  (when (and v (not (equal v s)) (not (shadowed-p v u)))
                    (fail "export: NAME-CONFLICT-IN-EXPORT of ~A from ~A ~
against ~S in user ~A" n pkg v u))))
              (setf (gethash (cons pkg n) present) (cons s :external))))))
      ;; Safeguarded check: present via INTERN-LOCAL-SOFT and in a
      ;; wired/safeguarded zone (IMach ACTUAL-STORAGE-CATEGORY = vma zone).
      (dolist (def *cold-package-defs*)
        (let ((pkg (getf def :pkg)))
          (dolist (n (getf def :safeguarded))
            (cond ((member n '("NIL" "T") :test #'string=)) ; wired blocks
                  ((not (gethash (cons pkg n) present))
                   (fail "safeguard: ~A not present in ~A" n pkg))
                  (t
                   (let ((vma (gethash (cons n pkg)
                                       (cold-world-symbols w))))
                     (unless (and vma (>= vma #xF0000000))
                       (fail "safeguard: ~A:~A at ~:[no vma~;#x~:*~X~] ~
is not in a safeguarded/wired zone" pkg n vma)))))))))
    (cold-check (null fails)
                "package boot simulation clean (~D failures~@[; first: ~A~])"
                (length fails) (first (last fails)))))

(defun check-embedded-network-functions (w)
  "M3h gate: the recompiled emb-ethernet-driver entries initialize-disk
and the periodic timer call pre-banner are real compiled functions (not
the interim IGNORE aliases, which LISP:IGNORE's cell would equal)."
  (let ((dtp-cf (cold-dtp w "COMPILED-FUNCTION")))
    (multiple-value-bind (itag idata)
        (cw-ref w (cold-follow-cell
                   w (+ (cold-vsym w (make-vsym "LISP" "IGNORE")) 2)))
      (declare (ignore itag))
      (dolist (name '("INITIALIZE-EMBEDDED-NETWORK"
                      "EMB-ETHERNET-PERIODIC-TIMER-FUNCTION"))
        (multiple-value-bind (tag data)
            (cw-ref w (cold-follow-cell
                       w (+ (cold-vsym w (make-vsym "NETWORK-INTERNALS"
                                                    name))
                            2)))
          (cold-check (and (= (tag-type tag) dtp-cf) (/= data idata))
                      "NETI:~A is a real compiled function (~2,'0X:~8,'0X)"
                      name tag data))))))

(defun check-reserved-regions (w reference)
  "M3h gate: the three reserved wired regions occupy the distribution's
region-table rows (14/15/16) with its origins, lengths and bits, and the
%<AREA>-REGION{,-ORIGIN,-LENGTH} stamps carry the distribution values."
  (loop for (area-name number) in '(("WIRED-DYNAMIC-AREA" 14)
                                    ("PAGE-TABLE-AREA" 15)
                                    ("GC-TABLE-AREA" 16))
        do (let ((region (cold-area-current-region w area-name)))
             (cold-check (and region (= (cold-region-number region) number))
                         "~A is region ~D" area-name number))
           (dolist (key '(:region-quantum-origin :region-quantum-length
                          :region-bits))
             (let ((vma (+ (cold-machinery w key) 1 number)))
               (multiple-value-bind (gt gd) (cw-ref w vma)
                 (multiple-value-bind (rt rd) (world-q reference vma)
                   (cold-check (and rt (= gt rt) (= gd rd))
                               "~A ~S: ~2,'0X:~8,'0X vs ref ~
~:[unmapped~;~:*~2,'0X:~8,'0X~]" area-name key gt gd rt rd)))))
           (dolist (suffix '("-REGION" "-REGION-ORIGIN" "-REGION-LENGTH"))
             (let ((sname (format nil "%~A~A" area-name suffix)))
               (multiple-value-bind (gt gd boundp)
                   (cold-symbol-value-q w (make-vsym "STORAGE" sname))
                 (multiple-value-bind (rt rd)
                     (reference-symbol-value reference sname)
                   (cold-check (and boundp rt
                                    (= (tag-type gt) (tag-type rt))
                                    (= gd rd))
                               "~A: ~2,'0X:~8,'0X vs ref ~
~:[missing~;~:*~2,'0X:~8,'0X~]" sname gt gd rt rd))))))
  ;; The region allocator's scalars: active count = this world's region
  ;; count, free list empty (bit 15 set per REGION-VALID-P).
  (let ((fixnum (cold-dtp w "FIXNUM")))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q
         w (make-vsym "SYSTEM-INTERNALS" "*NUMBER-OF-ACTIVE-REGIONS*"))
      (cold-check (and boundp (= (tag-type tag) fixnum)
                       (= data (fill-pointer (cold-world-regions w))))
                  "*NUMBER-OF-ACTIVE-REGIONS* = ~@[~D~] (region count ~D)"
                  (and boundp data) (fill-pointer (cold-world-regions w))))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS" "*FREE-REGION*"))
      (cold-check (and boundp (= (tag-type tag) fixnum)
                       (logbitp 15 data))
                  "*FREE-REGION* is a bit-15-set fixnum (empty free list)"))))

(defun check-wired-machinery (w reference)
  "M3e gate: magic-location forwarding for all three comm blocks, the
storage tables at ground-truth addresses describing this world's regions,
the initial stack group per MAKE-STACK-GROUP's invariants, and the trap
page fully reconciled against the reference."
  (with-cold-checks ("wired machinery")
    (let ((layout (cold-world-layout w)))
      ;; Reserved tables landed at the distribution addresses.
      (loop for (key address) in *cold-machinery-table-addresses*
            do (cold-check (= (cold-machinery w key) address)
                           "~S at #x~8,'0X, ground truth #x~8,'0X"
                           key (cold-machinery w key) address))
      ;; Three blocks, forwarding + stash/layout agreement.
      (cold-check (= (length (cold-world-magic w)) 3)
                  "~D magic blocks stashed" (length (cold-world-magic w)))
      (dolist (block (layout-section layout :magic-locations))
        (let* ((suffix (strip-package (first block)))
               ;; Layout: SYSTEM-COMMUNICATION-AREA etc; stash names match.
               (stash (cold-magic-block w suffix)))
          (check-magic-forwarding w block stash)))
      ;; BOOT-COMM pages must not exist in the world file.
      (cold-check (not (nth-value 2 (cw-ref w #xFFFE0000)))
                  "BOOT-COMM page #xFFFE0000 must stay host-owned")
      (check-syscom-slots w #xF8041100)
      (check-machinery-region-tables w)
      (check-initial-stack-group w)
      ;; No magic symbol may also sit in the wired symbol-cell table: its
      ;; boot-time re-forwarding would undo the comm-slot forward.
      (let ((magic-cells (make-hash-table)))
        (dolist (stash (cold-world-magic w))
          (dolist (var (third stash))
            (setf (gethash (cold-magic-var-cell w var) magic-cells) t)))
        (let* ((tbl (cold-world-wired-cell-table w))
               (back (nth-value 1 (cw-ref w (- tbl 3))))
               (collisions 0))
          (dotimes (i (cold-world-wired-cell-fill w))
            (multiple-value-bind (tag data) (cw-ref w (+ back 1 i))
              (declare (ignore tag))
              (when (or (gethash (+ data 1) magic-cells)
                        (gethash (+ data 2) magic-cells))
                (incf collisions))))
          (cold-check (zerop collisions)
                      "~D magic symbols also in the wired cell table"
                      collisions)))
      (when reference
        (check-trap-page-against-reference w reference)
        (check-magic-table-vs-reference w reference #xF8041100)
        (check-fepcomm-grafts w reference)
        (check-fepcomm-boot-stamps w reference)
        (check-wired-arrays w reference)
        (check-disk-events w reference)
        (check-reserved-regions w reference))
      (check-area-list w)
      (check-allocator-tables w)
      (check-embedded-network-functions w))))

;;; M3f gate: finalize, emit, re-read, audit.

(defun fspec-key-name (key)
  "The NAME part of a symbol fspec key (\"PKG:NAME\"); NIL for list keys."
  (when (stringp key)
    (let ((colon (position #\: key)))
      (if colon (subseq key (1+ colon)) key))))

(defun cold-unbound-function-cells (w)
  "R1 audit: fspecs whose (forward-followed) definition cell is still
dtp-null at emit -- everything the cold load referenced (function-cell
locatives, trap vectors, fdefine targets) that never got a definition --
minus the boot stubs (FSET at first boot, cold-load.lisp:131).  Sorted
key strings."
  (let ((dtp-null (cold-dtp w "NULL")) (rows nil))
    (maphash
     (lambda (key cell)
       (multiple-value-bind (tag data)
           (cw-ref w (cold-follow-cell w cell))
         (declare (ignore data))
         (when (and (= (tag-type tag) dtp-null)
                    (not (member (fspec-key-name key)
                                 *cold-boot-stub-functions*
                                 :test #'equal)))
           (push (if (stringp key) key (format nil "~S" key)) rows))))
     (cold-world-fdefs w))
    (sort rows #'string<)))

(defun cold-unbound-value-cells (w)
  "R2 audit: symbols whose value cell is locative-referenced somewhere in
the loaded world, still unbound (forward-followed dtp-null) at emit, and
not repaired by the boot's own *COLD-LOAD-VARIABLE-INITIALIZATIONS* loop
(cold-load.lisp:526).  Sorted \"PKG:PNAME\" strings.  A locative does not
say whether its instruction reads, writes, or binds (that needs
instruction decoding), so rows are review candidates, not certain traps;
the reviewed classification is *COLD-REVIEWED-UNBOUND-VALUE-CELLS*."
  (let ((dtp-null (cold-dtp w "NULL"))
        (dtp-loc (cold-dtp w "LOCATIVE"))
        (dtp-list (cold-dtp w "LIST"))
        (dtp-symbol (cold-dtp w "SYMBOL"))
        (key-of (make-hash-table))
        (referenced (make-hash-table))
        (init (make-hash-table)))
    (maphash (lambda (key vma)
               (setf (gethash vma key-of)
                     (format nil "~A:~A" (cdr key) (car key))))
             (cold-world-symbols w))
    (maphash (lambda (pageno qv)
               (declare (ignore pageno))
               (dotimes (i +ivory-page-size-qs+)
                 (multiple-value-bind (tag data) (qref qv i)
                   (when (= (tag-type tag) dtp-loc)
                     (let ((sym (1- data)))
                       (when (gethash sym key-of)
                         (setf (gethash sym referenced) t)))))))
             (cold-world-pages w))
    (multiple-value-bind (tag data boundp)
        (cold-symbol-value-q
         w (si-vsym "*COLD-LOAD-VARIABLE-INITIALIZATIONS*"))
      (when (and boundp (= (tag-type tag) dtp-list))
        (cold-map-list
         w tag data
         (lambda (et ed evma)
           (declare (ignore evma))
           (when (= (tag-type et) dtp-list)
             (multiple-value-bind (ct cd)
                 (cw-ref w (cold-follow-cell w ed))
               (when (= (tag-type ct) dtp-symbol)
                 (setf (gethash cd init) t))))
           nil))))
    (let ((rows nil))
      (maphash
       (lambda (vma key)
         (when (and (gethash vma referenced)
                    (not (gethash vma init)))
           (multiple-value-bind (tag data)
               (cw-ref w (cold-follow-cell w (1+ vma)))
             (declare (ignore data))
             (when (= (tag-type tag) dtp-null)
               (push key rows)))))
       key-of)
      (sort rows #'string<))))

(defparameter *cold-reviewed-unbound-value-cells*
  '(
    "CLOS-INTERNALS:*DECL-TYPES-INHERITED-FROM-METHOD*"
    ;; Flavor runtime (M3h boot 26): argless (DEFVAR v) dynamic
    ;; bindings.  *TRANSFORM-FLAVOR-WARNINGS* is LET-bound by its own
    ;; wrapper macro (defflavor.lisp:390); the three *COMBINED-METHOD-*
    ;; specials are MULTIPLE-VALUE-BIND targets during combined-method
    ;; construction (combine.lisp:850,1091).  Never read unbound.
    "FLAVOR:*COMBINED-METHOD-APPLY*"
    "FLAVOR:*COMBINED-METHOD-ARGUMENTS*"
    "FLAVOR:*COMBINED-METHOD-LAMBDA-LIST*"
    "FLAVOR:*TRANSFORM-FLAVOR-WARNINGS*"
    "COMMON-LISP-INTERNALS:*ALL-EMB-POOLS*"
    "COMMON-LISP-INTERNALS:*COMM-AREA-NEXT-FREE-OFFSET*"
    "COMMON-LISP-INTERNALS:*COMM-AREA-TOP-OFFSET*"
    "COMMON-LISP-INTERNALS:*EMB-HANDLE-ARRAY-NEXT-FREE*"
    "COMMON-LISP-INTERNALS:*EMB-POOL-COUNT*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-MODE-METERS*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-MODE-TIME*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-MODE*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-TASK-CACHE*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-TASK-FREE-LIST*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-TASK-HEADS*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-TASK-PRIORITY*"
    "COMMON-LISP-INTERNALS:*INTERRUPT-TASK-TAILS*"
    "COMMON-LISP-INTERNALS:*MAX-RUN-LIGHT-OFFSET*"
    "COMMON-LISP-INTERNALS:*MIN-RUN-LIGHT-OFFSET*"
    "COMMON-LISP-INTERNALS:*TIMER-MULTIPLE*"
    "COMMON-LISP-INTERNALS:*TIMER-PERIOD*"
    "COMMON-LISP-INTERNALS:*TIMER-PHASE*"
    "COMPILER:*BINARY-OUTPUT-STREAM*"
    "COMPILER:*COMPILE-FUNCTION*"
    "COMPILER:DEFAULT-WARNING-DEFINITION-TYPE"
    "COMPILER:DEFAULT-WARNING-FUNCTION"
    "COMPILER:QC-FILE-READ-IN-PROGRESS"
    "DEBUGGER:.RESTART.DESCRIPTION."
    "DEBUGGER:*TRAP-DISPATCH-TABLE*"
    "DEBUGGER:REPORT-IGNORED-ERRORS"
    "DYNAMIC-WINDOWS:*ACCEPT-ACTIVATION-CHARS*"
    "DYNAMIC-WINDOWS:*ACCEPT-ACTIVE*"
    "DYNAMIC-WINDOWS:*ACCEPT-BLIP-CHARS*"
    "DYNAMIC-WINDOWS:*ACCEPT-HELP*"
    "FORMAT:*COMMON-LISP-FORMAT*"
    "FUTURE-COMMON-LISP:CONDITION"
    "GLOBAL:PKG-GLOBAL-PACKAGE"
    "GLOBAL:PKG-SYSTEM-PACKAGE"
    "GLOBAL:RETURN-LIST"
    "LANGUAGE-TOOLS:*SIMPLE-VARIABLES*"
    "LISP:*DEBUG-IO*"
    "LISP:*ERROR-OUTPUT*"
    "LISP:*QUERY-IO*"
    "LISP:*READTABLE*"
    "LISP:*STANDARD-INPUT*"
    "LISP:*STANDARD-OUTPUT*"
    "LISP:*TERMINAL-IO*"
    "LISP:*TRACE-OUTPUT*"
    "METERING:*ENABLE-METERING-ON-FUNCTION-CALLS*"
    "METERING:WIRED-METERING-AREA"
    "NETWORK-INTERNALS:*EMB-ETHERNET-INTERFACES*"
    "NETWORK-INTERNALS:*N-EMB-ETHERNET-INTERFACES*"
    "NETWORK-INTERNALS:*PKTS-ALLOCATED*"
    "NETWORK-INTERNALS:%ETHER-BUFFER-AREA-REGION"
    "STORAGE:*ACTIVE-STACK-GROUPS-HEAD*"
    "STORAGE:*ACTIVE-STACK-GROUPS-TAIL*"
    "STORAGE:*COUNT-ACTIVE-STACK-GROUPS*"
    "STORAGE:*CREATING-DYNAMIC-SPACE*"
    "STORAGE:*FLUSHABLE-SCAN-PHT-INDEX*"
    "STORAGE:*INHIBIT-READ-ONLY-IN-PROGRESS*"
    "STORAGE:*LAST-AUX-PAGE-FAULT-VMA*"
    "STORAGE:*PAGE-FAULT-CONTROL-REGISTER*"
    "STORAGE:*PAGE-FAULT-DEPTH*"
    "STORAGE:*PAGE-FAULT-FRAME-POINTER*"
    "STORAGE:*PAGE-FAULT-PROGRAM-COUNTER*"
    "STORAGE:*READ-ONLY-N-WRITTEN-PAGES*"
    "STORAGE:*SMPT-CACHED-VPN*"
    "STORAGE:*TRANSPORTER-READ-ONLY-VPN*"
    "STORAGE:*USER-ANONYMOUS-DISK-EVENT*"
    "STORAGE:*USER-ROOT-DISK-EVENT*"
    "STORAGE:*USER-SERIAL-DISK-EVENT*"
    "STORAGE:*WIRE/UNWIRE-TICK*"
    "STORAGE:*WIRED-CONTROL-STACK-PAGES*"
    "SYMBOLICS-COMMON-LISP:ARGLIST"
    "SYSTEM-INTERNALS:****"
    "SYSTEM-INTERNALS:*COLD-BOOT-MICROSECOND-TIME-HIGH*"
    "SYSTEM-INTERNALS:*COLD-BOOT-MICROSECOND-TIME-LOW*"
    "SYSTEM-INTERNALS:*COLD-LOAD-DUPLICATED-SYMBOLS*"
    "SYSTEM-INTERNALS:*COLD-LOAD-STREAM-RUBOUT-HANDLER-BUFFER*"
    "SYSTEM-INTERNALS:*COLD-LOADED-FILE-PROPERTY-LISTS*"
    "SYSTEM-INTERNALS:*COUNT*"
    "SYSTEM-INTERNALS:*CURRENT-SELF-EVALUATING-SYMBOL-TABLE*"
    "SYSTEM-INTERNALS:*ECHOPLEX*"
    "SYSTEM-INTERNALS:*FORWARDED-SYMBOL-CELL-TABLE-LOCK*"
    "SYSTEM-INTERNALS:*HIGH-PART*"
    "SYSTEM-INTERNALS:*INDEX*"
    "SYSTEM-INTERNALS:*INPUT-HISTORY-DEFAULT*"
    "SYSTEM-INTERNALS:*INSTANT-PACKAGE-DWIM-MODULUS*"
    "SYSTEM-INTERNALS:*INSTANT-PACKAGE-DWIM-PACKAGE*"
    "SYSTEM-INTERNALS:*INSTANT-PACKAGE-DWIM-TABLE*"
    "SYSTEM-INTERNALS:*IOCH"
    "SYSTEM-INTERNALS:*IOLST"
    "SYSTEM-INTERNALS:*IVORY-REVISION-NUMBER*"
    "SYSTEM-INTERNALS:*LISP-PACKAGE*"
    "SYSTEM-INTERNALS:*LOCAL-DECLARATIONS-CACHE*"
    "SYSTEM-INTERNALS:*LOCAL-DECLARATIONS-LAST-LOCAL-DECLARATIONS*"
    "SYSTEM-INTERNALS:*LOW-PART*"
    "SYSTEM-INTERNALS:*NUMERIC-ARG-P*"
    "SYSTEM-INTERNALS:*NUMERIC-ARG*"
    "SYSTEM-INTERNALS:*OPEN-BRACKET*"
    "SYSTEM-INTERNALS:*PACKAGE-NAME-AARRAY*"
    "SYSTEM-INTERNALS:*PACKAGE-NAME-TABLE-COUNT*"
    "SYSTEM-INTERNALS:*PACKAGE-NAME-TABLE-MODULUS*"
    "SYSTEM-INTERNALS:*POWER-10*"
    "SYSTEM-INTERNALS:*PRINT-ERROR*"
    "SYSTEM-INTERNALS:*PROMPT-AND-READ-ECHO*"
    "SYSTEM-INTERNALS:*PROMPT-AND-READ-VISIBLE-SUFFIX*"
    "SYSTEM-INTERNALS:*QLD-MESSAGES*"
    "SYSTEM-INTERNALS:*READ-CIRCULARITY-UNRESOLVED-LABELS*"
    "SYSTEM-INTERNALS:*READ-CIRCULARITY*"
    "SYSTEM-INTERNALS:*RESCAN-STATE*"
    "SYSTEM-INTERNALS:*SCAVENGE-IN-PROGRESS*"
    "SYSTEM-INTERNALS:*SCL-PACKAGE*"
    "SYSTEM-INTERNALS:*SIMPLE-LISTENER-PROCESS*"
    "SYSTEM-INTERNALS:*STANDARD-CHARACTER-SET*"
    "SYSTEM-INTERNALS:*SYSTEM-SYMBOL-CELL-TABLE-TAIL*"
    "SYSTEM-INTERNALS:*SYSTEM-SYMBOL-CELL-TABLE*"
    "SYSTEM-INTERNALS:*USER-PACKAGE*"
    "SYSTEM-INTERNALS:++++"
    "SYSTEM-INTERNALS:APROPOS-SUBSTRING"
    "SYSTEM-INTERNALS:FDEFINE-FILE-DEFINITIONS"
    "SYSTEM-INTERNALS:GC-FLIP-INHIBIT-WAIT-TIME"
    "SYSTEM-INTERNALS:INFERIOR-INTERVAL"
    "SYSTEM-INTERNALS:MINI-EOF-SEEN"
    "SYSTEM-INTERNALS:MINI-LOCAL-INDEX"
    "SYSTEM-INTERNALS:MINI-OPEN-FILE"
    "SYSTEM-INTERNALS:MINI-OPEN-P"
    "SYSTEM-INTERNALS:MINI-PACKET"
    "SYSTEM-INTERNALS:MINI-PACKET-INDEX"
    "SYSTEM-INTERNALS:MINI-PACKET-MAX"
    "SYSTEM-INTERNALS:MINI-PACKET-NUMBER-IN"
    "SYSTEM-INTERNALS:MINI-PACKET-NUMBER-OUT"
    "SYSTEM-INTERNALS:MINI-PACKET-OPCODE"
    "SYSTEM-INTERNALS:MINI-PACKET-STRING"
    "SYSTEM-INTERNALS:MINI-REMOTE-INDEX"
    "SYSTEM-INTERNALS:MINI-UNIQUE-ID"
    "SYSTEM-INTERNALS:PKG-CODE-SYMBOLS"
    "SYSTEM-INTERNALS:PKG-FONTS-PACKAGE"
    "SYSTEM-INTERNALS:PKG-NETWORK-PACKAGE"
    "SYSTEM-INTERNALS:PKG-SYSTEM-INTERNALS-PACKAGE"
    "SYSTEM-INTERNALS:PKG-USER-PACKAGE"
    "SYSTEM-INTERNALS:REHASH-THESE-HASH-TABLES-BEFORE-COLD"
    "SYSTEM-INTERNALS:SETSYNTAX-FUNCTION"
    "SYSTEM-INTERNALS:SETSYNTAX-SHARP-MACRO-CHARACTER"
    "SYSTEM-INTERNALS:SETSYNTAX-SHARP-MACRO-FUNCTION"
    "SYSTEM-INTERNALS:SORT-ARRAY-TEMP-V"
    "SYSTEM-INTERNALS:SORT-DUMMY-ARRAY-HEADER"
    "SYSTEM-INTERNALS:SORT-INPUT-LIST"
    "SYSTEM-INTERNALS:SORT-LESSP-PREDICATE"
    "SYSTEM-INTERNALS:SORT-LESSP-PREDICATE-ON-CAR"
    "SYSTEM-INTERNALS:WARM-BOOTED-PROCESSES"
    "SYSTEM-INTERNALS:XR-SHARP-ARGUMENT"
    "SYSTEM:*DISK-UNIT-TABLE*"
    "SYSTEM:*LISP-STATE-SAVED*"
    "SYSTEM:*LISP-STOPPED-CLEANLY*"
    "SYSTEM:*PACKAGE-NAME-TABLE*"
    "SYSTEM:NET-ADDRESS-1"
    "SYSTEM:NET-ADDRESS-2"
    "SYSTEM:PKG-KEYWORD-PACKAGE"
    "SYSTEM:SYN-TERMINAL-IO"
    "TIME:*BOOT-MICROSECOND-TIME-HIGH*"
    "TIME:*BOOT-MICROSECOND-TIME-LOW*"
    "TV:*ACTIVE-WHO-LINE-SCREENS*"
    "TV:*CURRENT-PROGRESS-NOTE*"
    "TV:*FORCIBLY-SHOW-PROGRESS-NOTES*"
    "TV:KBD-LAST-ACTIVITY-TIME"
    "TV:MAIN-SCREEN"
    "TV:WHO-LINE-RUN-LIGHT-LOC"
    "TV:WHO-LINE-RUN-STATE"
    "TV:WHO-LINE-RUN-STATE-SHEET"
    "ZWEI:*INTERVAL*"
    "ZWEI:*KILL-HISTORY-USER*"
    "ZWEI:*KILL-HISTORY*"
    "ZWEI:*MODE-LIST-SYNTAX-TABLE*"
    )
  "R2 danger set as reviewed 2026-07-05 (M3h boot 18; boot 19 re-homed
CONDITIONS:CONDITION to FUTURE-COMMON-LISP, its pkgdcl exporter -- same
symbol, same warm-only classification).  Every entry was
classified against its source: bind-before-read (LET/argument binding of
a special -- SORT-*, XR-*, SETSYNTAX-*, *PRINT-ERROR*, IGNORE-ERRORS'
DBG:REPORT-IGNORED-ERRORS, METERING's flag, FORMAT:*COMMON-LISP-FORMAT*),
VARIABLE-BOUNDP-guarded (FDEFINE-FILE-DEFINITIONS, *INSTANT-PACKAGE-DWIM-*,
SYS:SYN-TERMINAL-IO), write-before-read on the boot path
(BUILD-INITIAL-PACKAGES' SETQs, BOOTSTRAP-FORWARD-SYMBOL-CELLS' tables,
RESET-COLD-BOOT-HISTORY, TV:KBD-LAST-ACTIVITY-TIME),
forwarded-then-set (LISP:*TERMINAL-IO* et al. via permanent-links;
boot 21 added LISP:*READTABLE* -- its eager LDATA defvar stamp is now
reverted so BOOTSTRAP-LINK-SYMBOL-CELLS can copy READTABLE's value in,
and nothing READs before that link pass),
deferred-covered (*COLD-LOAD-STREAM-RUBOUT-HANDLER-BUFFER*), warm-only
(ZWEI/TV/DW/MINI/QLD/compiler/CLOS...), or wired/embedding registers that
trap handlers and the init code boots 1-18 already exercised write before
anything reads (STORAGE:*PAGE-FAULT-*, CLI:*INTERRUPT-TASK-*,
TIME:*BOOT-MICROSECOND-*; their value cells are one-q-forwards into the
wired tables).  The gate fails on ANY drift --
a new entry is a potential boot trap to review; a vanished entry means
this list is stale.")

(defun check-unbound-value-cells (w)
  "M3h boot-18 gate: the R2 danger set matches the reviewed
classification exactly."
  (let* ((rows (cold-unbound-value-cells w))
         (new (set-difference rows *cold-reviewed-unbound-value-cells*
                              :test #'equal))
         (gone (set-difference *cold-reviewed-unbound-value-cells* rows
                               :test #'equal)))
    (cold-check (null new)
                "R2: no unreviewed referenced-unbound value cells ~
(~D new: ~S)" (length new) new)
    (cold-check (null gone)
                "R2: reviewed unbound-value-cell list is current ~
(~D stale: ~S)" (length gone) gone)))

(defun check-cold-emit (w tmpdir reference)
  "Finalize the loaded world, emit fresh.ilod with the wired/unwired map
split, re-read it, and check the boot-critical Qs on the FILE.  Also
prints the R1 unbound-function-cell audit."
  (with-cold-checks ("cold emit (fresh world)")
    (multiple-value-bind (ndeferred npatches npackages)
        (handler-case (cold-finalize w)
          (error (e)
            (cold-check nil "finalize: ~A" e)
            (values nil nil nil)))
      (when ndeferred
        (format t "  deferred list: ~D forms (~D patches ahead), ~
~D packages~%" ndeferred npatches npackages)
        (cold-check (<= 90 npackages 120) "~D packages" npackages)
        ;; The three finalize-owned variables, in the world.
        (multiple-value-bind (tag data boundp)
            (cold-symbol-value-q
             w (make-vsym "SYSTEM-INTERNALS" "*COLD-LOAD-DEFERRED-FORMS*"))
          (declare (ignore data))
          (cold-check (and boundp (= (tag-type tag) (cold-dtp w "LIST")))
                      "*COLD-LOAD-DEFERRED-FORMS* is a bound list"))
        (multiple-value-bind (tag data boundp)
            (cold-symbol-value-q
             w (make-vsym "SYSTEM-INTERNALS" "BUILD-INITIAL-PACKAGES"))
          (declare (ignore data))
          (cold-check (and boundp (= (tag-type tag) (cold-dtp w "LIST")))
                      "BUILD-INITIAL-PACKAGES is a bound list"))
        (multiple-value-bind (tag data boundp)
            (cold-symbol-value-q
             w (make-vsym "SYSTEM-INTERNALS" "*VALUE-CELLS-TO-LOCALIZE-FIRST*"))
          (cold-check (and boundp (cold-q-nil-p w tag data))
                      "*VALUE-CELLS-TO-LOCALIZE-FIRST* is NIL"))
        ;; *LINKED-SYMBOL-CELLS* mirrors the permanent-links load pass;
        ;; runs here because finalize materializes it just above.
        (check-linked-symbol-cells w)
        ;; ... and no record may ship two bound-but-different cells, or
        ;; BOOTSTRAP-LINK-SYMBOL-CELLS FERRORs at first boot (M3h boot 21).
        (check-bootstrap-link-invariant w)
        ;; Every plist NIL-terminated with intact cdr codes; link-record
        ;; symbols additionally NULL-free -- DECLARED-STORAGE-CATEGORY
        ;; GETs their plists at first boot (M3h boot 23).
        (check-plist-termination w)
        ;; The boot's region object walks must parse every Q up to each
        ;; free pointer (M3h boot 24).
        (check-boot-object-walk w)
        (check-pass1-fspec-handlers w)
        ;; Keyword self-evaluation forwarding, also materialized by finalize.
        (check-keyword-self-eval w)
        ;; :RELATIVE-NAMES withheld from the package calls, deferred as
        ;; SI:PKG-ADD-RELATIVE-NAME forms (M3h boot 14).
        (check-relative-name-deferral w)
        ;; Dialect-split symbols (GLOBAL:FORMAT vs LISP:FORMAT) resolve
        ;; through the pkgdcl package graph (M3h boot 15).
        (check-split-symbol-resolution w)
        ;; NIL/T architectural blocks fully stamped (M3h boot 16).
        (check-nil-t-stamp w)
        ;; No symbol homed in a package BUILD-INITIAL-PACKAGES won't
        ;; create (M3h boot 17).
        (check-declared-package-homes w)
        ;; No symbol homed below a pkgdcl exporter of its pname
        ;; (M3h boot 19).
        (check-export-home-conflicts w)
        ;; Full BUILD-INITIAL-PACKAGES simulation: sweep + shadow +
        ;; import-export + safeguarded, exact package.lisp conflict
        ;; predicates (M3h boot 20).
        (check-package-boot-simulation w)
        ;; The safeguarded pre-intern block mirrors the dist: KEYWORD's
        ;; clause first at #xF0001054, right after the storage tables.
        (cold-check (eql (gethash (cons "OBS" "KEYWORD")
                                  (cold-world-symbols w))
                         #xF0001054)
                    "safeguarded symbol block starts at KEYWORD:OBS ~
#xF0001054 (dist ground truth)")
        ;; R2: referenced-unbound value cells all reviewed (M3h boot 18).
        (check-unbound-value-cells w)
        ;; Emit with the map split and re-read.
        (let ((out (format nil "~A/fresh.ilod" tmpdir))
              (model (cold-world-model
                      w :wired-ranges (cold-wired-ranges w))))
          (format t "  map: ~D wired + ~D unwired entries~%"
                  (length (world-model-wired-map model))
                  (length (world-model-unwired-map model)))
          (cold-check (plusp (length (world-model-unwired-map model)))
                      "heap pages are unwired")
          (write-file-bytes out (write-world model))
          (let ((fresh (read-world out)))
            ;; The Q the emulator boots through (interfac.c:775).
            (multiple-value-bind (tag data)
                (world-q fresh (+ #xF8041100 2))
              (cold-check (and tag
                               (= (tag-type tag)
                                  (cold-dtp w "COMPILED-FUNCTION")))
                          "fresh.ilod SYSCOM+2 (systemStartup) tag ~
#x~2,'0X, need #x1C" (and tag (tag-type tag)))
              (when reference
                (multiple-value-bind (rt rd)
                    (world-q reference (+ #xF8041100 2))
                  (declare (ignore rd))
                  (cold-check (and tag (= (tag-type tag) (tag-type rt)))
                              "SYSCOM+2 tag matches reference")
                  data)))
            ;; NIL/T and the trap page live in the WIRED map of the file.
            (dolist (vma (list (cold-world-nil-vma w) #xF8040000))
              (cold-check
               (loop for e in (world-model-wired-map fresh)
                     thereis (and (<= (map-entry-address e) vma)
                                  (< vma (+ (map-entry-address e)
                                            (map-entry-count e)))))
               "vma #x~X wired in the file" vma))
            ;; Every trap-vector handler PC targets a WIRED page: the
            ;; page-fault path must never fault on its own handler.  PCs
            ;; into the FEP reservation (F8000000..F8040000) are exempt --
            ;; the grafted mode-3 vectors point into the IFEP kernel,
            ;; which the emulator loads itself (unmapped even in the
            ;; reference world).
            (let ((even-pc (cold-dtp w "EVEN-PC"))
                  (odd-pc (cold-dtp w "ODD-PC"))
                  (unwired-pcs 0))
              (dotimes (i 4096)
                (multiple-value-bind (tag data)
                    (world-q fresh (+ #xF8040000 i))
                  (when (and tag
                             (member (tag-type tag) (list even-pc odd-pc))
                             (not (<= #xF8000000 data (1- #xF8040000))))
                    (unless (loop for e in (world-model-wired-map fresh)
                                  thereis (and (<= (map-entry-address e) data)
                                               (< data
                                                  (+ (map-entry-address e)
                                                     (map-entry-count e)))))
                      (incf unwired-pcs)))))
              (cold-check (zerop unwired-pcs)
                          "trap page: ~D handler PCs on unwired pages"
                          unwired-pcs))
            ;; Byte-stable emit.
            (cold-check (equalp (write-world model) (write-world fresh))
                        "re-emit of the re-read world is byte-identical")))
        ;; R1 audit.
        (let ((rows (cold-unbound-function-cells w)))
          (format t "  R1 unbound function cells: ~D (boot stubs ~
excluded)~%" (length rows))
          (loop for name in rows
                for i from 0 below 10
                do (format t "    ~A~%" name))
          (when (> (length rows) 10)
            (format t "    ... ~D more (full list: coldgen writes ~
OUT.unbound-fcells.txt)~%" (- (length rows) 10))))))))

;;; Stage-test driver (CLI: worldtool coldtest TMPDIR [REFERENCE-WORLD])

(defun cold-test (tmpdir &key reference layout-path sysdir)
  "Build the current-stage cold world, emit, re-read, check.  Returns the
number of failed stages (0 = success)."
  (let* ((layout (read-layout layout-path))
         (failures 0)
         (w (make-skeleton-world layout))
         (out (format nil "~A/cold-skeleton.ilod" tmpdir))
         (model (cold-world-model w))
         (reference-model (when reference (read-world reference))))
    (multiple-value-bind (homes aliases)
        (if reference-model
            (world-symbol-homes reference-model)
            (values nil nil))
      (setf *cold-symbol-homes* homes
            *cold-package-aliases* aliases))
    (when *cold-symbol-homes*
      (format t "~&symbol-home oracle: ~D pnames, ~D package aliases~%"
              (hash-table-count *cold-symbol-homes*)
              (hash-table-count *cold-package-aliases*)))
    (write-file-bytes out (write-world model))
    (let ((reread (read-world out)))
      (unless (check-skeleton w reread :reference reference-model)
        (incf failures)))
    ;; M3b: materializers work against a fresh skeleton + heap regions.
    (let ((w2 (make-skeleton-world layout)))
      (cold-add-heap-regions w2)
      (unless (check-materializers w2)
        (incf failures))
      (when sysdir
        (setup-sys-host (if (char= (char sysdir (1- (length sysdir))) #\/)
                            sysdir
                            (concatenate 'string sysdir "/")))
        (format t "~&package graph: ~D pkgdcl packages~%"
                (cold-build-package-graph
                 (sys-pathname "SYS: SYS; PKGDCL" "lisp")))
        (let ((w3 (make-skeleton-world layout)))
          (cold-add-heap-regions w3)
          (unless (check-vbin-census w3 (sys-pathname "SYS: IO; RDDEFS"))
            (incf failures)))
        ;; M3c: every compiled function in the cold set, then the
        ;; SYSTEM-STARTUP oracle against the reference world.
        (let ((w4 (make-skeleton-world layout)))
          (cold-add-heap-regions w4)
          (unless (check-cold-set-vfuns w4 sysdir)
            (incf failures)))
        (when reference-model
          (let ((w5 (make-skeleton-world layout)))
            (cold-add-heap-regions w5)
            (unless (check-system-startup-oracle w5 reference-model)
              (incf failures))))
        ;; M3d: the vop dispatcher + mini-eval over the whole cold set.
        ;; M3e: the wired machinery pass on the same loaded world.
        ;; M3f: finalize + emit + audit, in the SAME materializer scope so
        ;; the deferred forms alias the load's materialized structure.
        (let ((w6 (make-skeleton-world layout)))
          (cold-add-heap-regions w6)
          (with-cold-materializer (w6)
            (unless (check-cold-eval w6 reference-model)
              (incf failures))
            (when reference-model
              (handler-case
                  (progn
                    (cold-build-wired-machinery w6 :reference reference-model)
                    (unless (check-wired-machinery w6 reference-model)
                      (incf failures)))
                (error (e)
                  (format t "~&wired machinery: ERROR ~A~%" e)
                  (incf failures)))
              (unless (check-cold-emit w6 tmpdir reference-model)
                (incf failures)))))))
    failures))

;;; Generator driver (CLI: worldtool coldgen LAYOUT OUT --reference R --sys D)

(defun coldgen (layout-path out &key reference sysdir)
  "Build a fresh world end to end and write OUT (.ilod) plus
OUT.unbound-fcells.txt (the full R1 audit).  REFERENCE (the unpatched
distribution world) and SYSDIR are required inputs: the symbol-home
oracle and the IFEP vector grafts come from the reference.  Returns 0."
  (let* ((layout (read-layout layout-path))
         (reference-model (read-world reference)))
    (multiple-value-bind (homes aliases) (world-symbol-homes reference-model)
      (setf *cold-symbol-homes* homes
            *cold-package-aliases* aliases))
    (setup-sys-host (if (char= (char sysdir (1- (length sysdir))) #\/)
                        sysdir
                        (concatenate 'string sysdir "/")))
    (format t "~&package graph: ~D pkgdcl packages~%"
            (cold-build-package-graph
             (sys-pathname "SYS: SYS; PKGDCL" "lisp")))
    (let ((w (make-skeleton-world layout))
          (*cold-eval-stats* (make-hash-table :test #'equal)))
      (multiple-value-bind (ndeferred npatches npackages)
          (cold-build-world w :reference reference-model)
        (format t "~&cold set loaded: ~D deferred forms (~D patches ~
ahead), ~D packages~%" ndeferred npatches npackages))
      (let* ((model (cold-world-model w :wired-ranges (cold-wired-ranges w)))
             (bytes (write-world model)))
        (write-file-bytes out bytes)
        (format t "~A: ~:D bytes, ~D wired + ~D unwired map entries~%"
                out (length bytes)
                (length (world-model-wired-map model))
                (length (world-model-unwired-map model))))
      ;; The Q the emulator boots through, on the file just written.
      (let ((fresh (read-world out)))
        (multiple-value-bind (tag data) (world-q fresh (+ #xF8041100 2))
          (declare (ignore data))
          (unless (and tag (= (tag-type tag)
                              (cold-dtp w "COMPILED-FUNCTION")))
            (error "~A: SYSCOM+2 tag #x~2,'0X -- the emulator requires ~
#x1C (compiled function)" out (and tag (tag-type tag))))))
      (let ((rows (cold-unbound-function-cells w))
            (report (concatenate 'string out ".unbound-fcells.txt")))
        (with-open-file (f report :direction :output :if-exists :supersede)
          (format f ";;; R1 audit: function cells the cold load references ~
that are still unbound at emit~%;;; (boot stubs from ~
*COLD-LOAD-FUNCTION-INITIALIZATIONS* excluded).~%")
          (dolist (r rows) (format f "~A~%" r)))
        (format t "R1 audit: ~D unbound referenced function cells -> ~A~%"
                (length rows) report))
      (let ((rows (cold-unbound-value-cells w))
            (report (concatenate 'string out ".unbound-vcells.txt")))
        (with-open-file (f report :direction :output :if-exists :supersede)
          (format f ";;; R2 audit: value cells the loaded world references ~
by locative that are still unbound at emit~%;;; (vars the boot's ~
*COLD-LOAD-VARIABLE-INITIALIZATIONS* loop repairs excluded).~%;;; ~
Entries marked UNREVIEWED are absent from ~
*COLD-REVIEWED-UNBOUND-VALUE-CELLS* -- review before booting.~%")
          (dolist (r rows)
            (format f "~A~@[  UNREVIEWED~]~%" r
                    (not (member r *cold-reviewed-unbound-value-cells*
                                 :test #'equal)))))
        (format t "R2 audit: ~D referenced unbound value cells (~D ~
unreviewed) -> ~A~%"
                (length rows)
                (count-if-not (lambda (r)
                                (member r *cold-reviewed-unbound-value-cells*
                                        :test #'equal))
                              rows)
                report)))
    0))
