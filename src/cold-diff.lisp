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
  '("PROCLAIM")
  "Defined in SYS:SYS;LISP-DATABASE-COLD, which the distribution cold load
contained but the M2 file list missed; its .vbin must be compiled in the
M2 Genera environment before M3f.  Accepted by the audit so the gate
tracks only NEW problems.")

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
  "M3d gate: full 85-file load with zero unhandled forms and zero
unresolved fixups; register-map ASETs took; the trap page carries real
vectors; SYSTEM-STARTUP is Q-for-Q EXACT against the reference
(CURRENT-DEFINITION-P included, courtesy of the fdefine handler)."
  (with-cold-checks ("cold eval (full cold set)")
    (let ((*cold-eval-stats* (make-hash-table :test #'equal))
          (failures nil)
          (fixup-failures 0))
      (handler-case
          (setf fixup-failures (cold-load-cold-set w))
        (error (e) (push (format nil "~A" e) failures)))
      (dolist (msg failures) (cold-check nil "load error: ~A" msg))
      (cold-check (zerop fixup-failures)
                  "~D unresolved fixups" fixup-failures)
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
        (let ((w6 (make-skeleton-world layout)))
          (cold-add-heap-regions w6)
          (unless (check-cold-eval w6 reference-model)
            (incf failures)))))
    failures))
