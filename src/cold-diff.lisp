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

(defun cold-read-string (w vma)
  "Read back a cold ART-STRING block (test aid)."
  (multiple-value-bind (tag data) (cw-ref w vma)
    (declare (ignore tag))
    (let* ((len (ldb (byte 15 0) data))
           (s (make-string len)))
      (dotimes (i len s)
        (multiple-value-bind (wt wd) (cw-ref w (+ vma 1 (floor i 4)))
          (declare (ignore wt))
          (setf (char s i)
                (code-char (ldb (byte 8 (* 8 (mod i 4))) wd))))))))

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
    "WARN" "FERROR" "ERROR" "MAKE-INSTANCE" "FIND-PACKAGE" "FIND-CLASS")
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
  "Walk every deferred form; flag call heads that are neither cold-defined,
stub-backed, FBOUNDP-guarded, nor interpreter special forms."
  (let ((bad (make-hash-table :test #'equal)))
    (loop for (pkg . form) in (cold-world-deferred w)
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
        (check-fepcomm-grafts w reference)))))

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
        (dolist (name '("*VALUE-CELLS-TO-LOCALIZE-FIRST*"
                        "*LINKED-SYMBOL-CELLS*"))
          (multiple-value-bind (tag data boundp)
              (cold-symbol-value-q w (make-vsym "SYSTEM-INTERNALS" name))
            (cold-check (and boundp (cold-q-nil-p w tag data))
                        "~A is NIL" name)))
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
                (length rows) report)))
    0))
