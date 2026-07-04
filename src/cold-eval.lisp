;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: vop dispatcher + load-time mini-eval.
;;;
;;; Every top-level event in a cold-set .vbin is either a vop (fdefine /
;;; setq / putprop / defconst / defvar) or a FORM eval.  This file routes
;;; each to one of four fates, mirroring what survives of the original
;;; generator's contract in the sources:
;;;
;;;   NATIVE   -- performed against cold memory now.  Everything the boot
;;;               path needs before *COLD-LOAD-DEFERRED-FORMS* is evaluated
;;;               (LISP-INITIALIZE-FIRST-TIME, sys/cold-load.lisp:547):
;;;               value/function cells, properties, trap vectors, wired
;;;               cell forwarding, register-map ASETs.  DEFVAR-1/DEFCONST-1/
;;;               DEFCONSTANT-1 are the three forms the sources mark
;;;               "also implemented by the cold-load generator"
;;;               (sys/lisp-database-cold.lisp).
;;;   DEFER    -- pushed (in load order) onto the deferred list, evaluated
;;;               at first boot BEFORE the banner with the cold-load stubs
;;;               installed (SPECIAL-LOAD-COLD, DEFCONSTANT-LOAD-2-COLD,
;;;               DEFGENERIC-INTERNAL-COLD ... cold-load.lisp:131).  Only
;;;               for heads that are fbound by then -- defined in a cold
;;;               file or stubbed.
;;;   GUARDED  -- deferred wrapped in (IF (FBOUNDP 'head) form): heads
;;;               whose owners load only during QLD (flavor system,
;;;               compiler, LOOP).  The registration is lost until the
;;;               owning system re-establishes it -- accepted gap, see the
;;;               plan file.  IF is interpreter-native; WHEN is a macro
;;;               that is NOT in the cold load, so it cannot appear here.
;;;   NO-OP    -- development-environment bookkeeping with no boot effect
;;;               and no boot-time definition (obsolescence style checkers,
;;;               compiler transformer edits).
;;;
;;; Unknown heads are a hard error: the routing table IS the design.

(in-package #:worldtool)

(defvar *cold-eval-stats*)              ; string -> count
(defvar *cold-eval-file* nil)           ; current file spec, for errors

(defun cold-note (key &optional (n 1))
  (when (boundp '*cold-eval-stats*)
    (incf (gethash key *cold-eval-stats* 0) n)))

;;; ---------------- Form utilities ----------------

(defun vsym-named-p (x name)
  (and (vsym-p x) (string= (vsym-name x) name)))

(defun form-head-name (form)
  (and (consp form) (vsym-p (first form)) (vsym-name (first form))))

(defun quoted (x)
  "If X is (QUOTE y), return (values y t)."
  (if (and (consp x) (vsym-named-p (first x) "QUOTE"))
      (values (second x) t)
      (values x nil)))

(defun si-vsym (name) (make-vsym "SYSTEM-INTERNALS" name))

;;; ---------------- Cold memory utilities ----------------

(defun cold-follow-cell (w vma)
  "Follow one-q-forward chains from VMA; returns the final cell vma."
  (loop for hops from 0 below 16
        do (multiple-value-bind (tag data) (cw-ref w vma)
             (if (= (tag-type tag) (cold-dtp w "ONE-Q-FORWARD"))
                 (setf vma data)
                 (return vma)))
        finally (return vma)))

(defun cold-value-cell (w vsym)
  (cold-follow-cell w (1+ (cold-vsym w vsym))))

(defun cold-symbol-value-q (w vsym)
  "(values tag data boundp) of VSYM's (forward-followed) value cell."
  (multiple-value-bind (tag data) (cw-ref w (cold-value-cell w vsym))
    (values tag data (/= (tag-type tag) (cold-dtp w "NULL")))))

(defun cold-set-symbol-value (w vsym vtag vdata)
  (cw-set w (cold-value-cell w vsym) vtag vdata))

(defun cold-q-nil-p (w tag data)
  (multiple-value-bind (ntag ndata) (cold-nil-q w)
    (and (= (tag-type tag) (tag-type ntag)) (= data ndata))))

(defun cold-q-eq (t1 d1 t2 d2)
  (and (= (tag-type t1) (tag-type t2)) (= d1 d2)))

(defun cold-cons (w cart card cdt cdd &optional (area "WORKING-STORAGE-AREA"))
  "A fresh 2-Q cons; returns its vma."
  (let ((vma (cold-alloc w area 2)))
    (cw-set w vma (logior (ash +cdr-normal+ 6) (tag-type cart)) card)
    (cw-set w (1+ vma) cdt cdd)
    vma))

(defun cold-map-list (w tag data fn)
  "Call FN with (values car-tag car-data cell-vma) for each cell of the
cdr-coded cold list Q TAG:DATA; stops early if FN returns non-NIL, and
returns that value."
  (loop with dtp-list = (cold-dtp w "LIST")
        while (= (tag-type tag) dtp-list)
        for guard from 0 below 100000
        do (let ((vma (cold-follow-cell w data)))
             (multiple-value-bind (ct cd) (cw-ref w vma)
               (let ((hit (funcall fn ct cd vma)))
                 (when hit (return hit)))
               (ecase (ash ct -6)
                 (0 (setf tag (tag 0 dtp-list) data (1+ vma)))
                 (1 (return nil))
                 ((2 3) (multiple-value-setq (tag data)
                          (cw-ref w (cold-follow-cell w (1+ vma))))))))))

;;; ---------------- Deferral ----------------

(defun cold-defer (w form &optional key)
  (cold-note (or key "deferred"))
  (push (cons *cold-default-package* form) (cold-world-deferred w)))

(defun cold-defer-guarded (w head-vsym form)
  (cold-note (format nil "guarded ~A" (vsym-name head-vsym)))
  (push (cons *cold-default-package*
              (list (si-vsym "IF")
                    (list (si-vsym "FBOUNDP")
                          (list (si-vsym "QUOTE") head-vsym))
                    form))
        (cold-world-deferred w)))

;;; ---------------- The value mini-eval ----------------
;;;
;;; (cold-eval-value w form) => (values tag data) on success,
;;; (values nil :defer) when the value only exists at boot (caller defers
;;; the whole enclosing form), (values nil :patch) when the caller should
;;; store NIL and queue a first-boot patch of the stored Q.

(defparameter *cold-patch-value-heads*
  '("FIND-RESOURCE" "FIND-GENERIC-FUNCTION-AS-CONSTANT"
    "LOAD-TIME-FIND-FLAVOR" "MAKE-MOUSE-CHAR"
    "DEFSELECT-CONS-WHICH-OPERATIONS")
  "Value forms whose result can only be computed at run time; the Q gets
NIL now and a first-boot %P-STORE-CONTENTS patch.")

(defun cold-eval-value (w form &key (area "PERMANENT-STORAGE-AREA"))
  (typecase form
    (veval (cold-eval-value w (veval-form form) :area area))
    (vsym
     (let ((pkg (canonical-package-name (vsym-package form))))
       (cond ((equal pkg "KEYWORD") (cold-symbol-ref w form))
             ((and (string= (vsym-name form) "NIL") pkg) (cold-nil-q w))
             ((and (string= (vsym-name form) "T") pkg)
              (values (tag 0 (cold-dtp w "SYMBOL")) (cold-world-t-vma w)))
             (t (multiple-value-bind (tag data boundp)
                    (cold-symbol-value-q w form)
                  (if boundp
                      (values tag data)
                      (values nil :defer)))))))
    (cons
     (let ((head (form-head-name form))
           (args (rest form)))
       (cond
         ((null head) (values nil :defer))
         ((string= head "QUOTE") (cold-ref w (first args) :area area))
         ((string= head "FUNCTION")
          (let ((cell (cold-follow-cell
                       w (cold-fdefinition-cell w (first args)))))
            (multiple-value-bind (tag data) (cw-ref w cell)
              (if (= (tag-type tag) (cold-dtp w "NULL"))
                  (values nil :defer)     ; caller may register a fixup
                  (values tag data)))))
         ((string= head "VALUES")
          (cold-eval-value w (first args) :area area))
         ((string= head "COPYTREE-AND-LEAVES")
          ;; (COPYTREE-AND-LEAVES 'tree area-symbol): load-time deep copy.
          ;; Host conses are fresh per decode, so a plain materialization
          ;; in the requested area already has copy semantics.
          (let ((tree (quoted (first args)))
                (target (if (vsym-p (second args))
                            (vsym-name (second args))
                            area)))
            (cold-ref w tree :area target)))
         ((and (= (length head) 7) (string= head "%LIST-" :end1 6))
          ;; (%LIST-n el ...): fixed-arity list of evaluated elements.
          (let ((qs (mapcar (lambda (a)
                              (multiple-value-bind (tag data)
                                  (cold-eval-value w a :area area)
                                (unless tag
                                  (return-from cold-eval-value
                                    (values nil data)))
                                (cons tag data)))
                            args)))
            (let ((vma (cold-alloc w "WORKING-STORAGE-AREA" (length qs))))
              (loop for (tag . data) in qs
                    for i from 0
                    for lastp = (= i (1- (length qs)))
                    do (cw-set w (+ vma i)
                               (logior (ash (if lastp +cdr-nil+ +cdr-next+) 6)
                                       (tag-type tag))
                               data))
              (values (tag 0 (cold-dtp w "LIST")) vma))))
         ((string= head "MAKE-AREA")
          ;; Load-time area creation; all 67 area numbers are architectural
          ;; facts recorded in the layout, so the value is just the number.
          (let ((name (loop for (k v) on args by #'cddr
                            when (vsym-named-p k "NAME")
                              return (quoted v))))
            (unless (vsym-p name)
              (error "MAKE-AREA without :NAME in ~S" form))
            ;; Area numbers are architectural facts in the layout's
            ;; :AREAS section (M2 exported all 67).
            (values (tag 0 (cold-dtp w "FIXNUM"))
                    (cold-area-number (cold-area w (vsym-name name))))))
         ((string= head "%MAKE-PC")
          ;; (%MAKE-PC function offset), sys/lcode.lisp:1060: offset in
          ;; halfword instructions; even/odd tag from its parity.
          (let ((fn-obj (first args))
                (offset (second args)))
            (unless (and (vfun-p fn-obj) (integerp offset))
              (error "Unsupported %MAKE-PC form ~S" form))
            (let ((fn (cold-fun w fn-obj)))
              (values (tag 0 (cold-dtp w (if (oddp offset) "ODD-PC" "EVEN-PC")))
                      (+ fn (floor offset 2))))))
         ((string= head "MEMBER-FAST")
          (multiple-value-bind (it id) (cold-eval-value w (first args))
            (unless it (return-from cold-eval-value (values nil id)))
            (multiple-value-bind (lt ld) (cold-eval-list-arg w (second args))
              (unless lt (return-from cold-eval-value (values nil ld)))
              (let ((hit (cold-map-list
                          w lt ld
                          (lambda (ct cd vma)
                            (when (cold-q-eq ct cd it id)
                              (cons (tag 0 (cold-dtp w "LIST")) vma))))))
                (if hit
                    (values (car hit) (cdr hit))
                    (cold-nil-q w))))))
         ((or (string= head "ADJOIN-FAST") (string= head "CONS"))
          (multiple-value-bind (it id) (cold-eval-value w (first args))
            (unless it (return-from cold-eval-value (values nil id)))
            (multiple-value-bind (lt ld) (cold-eval-list-arg w (second args))
              (unless lt (return-from cold-eval-value (values nil ld)))
              (when (string= head "ADJOIN-FAST")
                (let ((hit (cold-map-list
                            w lt ld
                            (lambda (ct cd vma)
                              (declare (ignore vma))
                              (when (cold-q-eq ct cd it id) t)))))
                  (when hit (return-from cold-eval-value (values lt ld)))))
              (values (tag 0 (cold-dtp w "LIST"))
                      (cold-cons w it id lt ld)))))
         ((string= head "NOT")
          (multiple-value-bind (tag data) (cold-eval-value w (first args))
            (unless tag (return-from cold-eval-value (values nil data)))
            (if (cold-q-nil-p w tag data)
                (values (tag 0 (cold-dtp w "SYMBOL")) (cold-world-t-vma w))
                (cold-nil-q w))))
         ((string= head "SETQ")
          (multiple-value-bind (tag data)
              (cold-eval-value w (second args))
            (unless tag (return-from cold-eval-value (values nil data)))
            (cold-set-symbol-value w (first args) tag data)
            (values tag data)))
         ((member head *cold-patch-value-heads* :test #'string=)
          (values nil :patch))
         (t (values nil :defer)))))
    ;; Self-evaluating host data (strings, numbers, characters, arrays...)
    (t (cold-ref w form :area area))))

(defun cold-eval-list-arg (w form)
  "Like COLD-EVAL-VALUE, but an UNBOUND variable reads as NIL: the
registration idioms (OR (MEMBER-FAST x VAR) (SETQ VAR (CONS x VAR))) run
in files loaded before VAR's (DEFVAR VAR NIL) -- e.g. ENCAPS pushes onto
SI:*ALL-FUNCTION-SPEC-HANDLERS* two files before FSPEC declares it.  The
later DEFVAR keeps the accumulated value (defvar stores only if unbound)."
  (multiple-value-bind (tag data) (cold-eval-value w form)
    (if (and (null tag) (eq data :defer) (vsym-p form))
        (cold-nil-q w)
        (values tag data))))

(defun cold-value-of-object (w obj &key (area "PERMANENT-STORAGE-AREA"))
  "Value Q of a decoded OPERAND OBJECT (quote semantics): only a veval
carries a form to evaluate; everything else materializes as itself.
Same return convention as COLD-EVAL-VALUE."
  (if (veval-p obj)
      (cold-eval-value w (veval-form obj) :area area)
      (cold-ref w obj :area area)))

;;; The cold-ref hook for eval operands nested inside data or instruction
;;; streams: value it if possible, otherwise store NIL and request a patch.
(defun cold-operand-eval (w form)
  (multiple-value-bind (tag data) (cold-eval-value w form)
    (cond (tag (values tag data))
          (t (cold-note "operand patches")
             (setf *cold-eval-patch-form* form)
             (cold-nil-q w)))))

;;; ---------------- defvar / defconst / defconstant ----------------

(defun cold-do-defvar (w kind sym value valuep doc localize
                       &key (value-kind :object))
  "KIND is :defvar (store only if unbound), :defconst or :defconstant
(always store).  VALUE-KIND :object for vop operands (quote semantics,
veval evaluated), :form for DEFVAR-1/DEFCONST-1 special-form arguments.
An unevaluable value defers a boot-time (SET 'sym form)."
  (unless (vsym-p sym)
    (error "~A of non-symbol ~S" kind sym))
  (cold-note (string-downcase (symbol-name kind)))
  ;; Specialness registers at boot through the SI:SPECIAL-LOAD stub.
  (cold-defer w (list (si-vsym "SPECIAL-LOAD")
                      (list (si-vsym "QUOTE") sym))
              "special-load")
  (when valuep
    (let ((store (or (not (eq kind :defvar))
                     (not (nth-value 2 (cold-symbol-value-q w sym))))))
      (multiple-value-bind (tag data)
          (if (eq value-kind :form)
              (cold-eval-value w value)
              (cold-value-of-object w value))
        (cond ((and tag store) (cold-set-symbol-value w sym tag data))
              (tag nil)                 ; defvar of an already-bound symbol
              (t
           (cold-note "deferred values")
           (let ((set-form (list (si-vsym "SET")
                                 (list (si-vsym "QUOTE") sym)
                                 (if (veval-p value) (veval-form value) value))))
             (cold-defer w (if (eq kind :defvar)
                               (list (si-vsym "IF")
                                     (list (si-vsym "BOUNDP")
                                           (list (si-vsym "QUOTE") sym))
                                     nil
                                     set-form)
                               set-form)
                         "deferred value forms")))))))
  (when (eq kind :defconstant)
    ;; Constant marking lands in *COLD-LOAD-CONSTANTS* via the boot stub.
    (cold-defer w (list (make-vsym "LANGUAGE-TOOLS" "DEFCONSTANT-LOAD-2")
                        (list (si-vsym "QUOTE") sym))
                "defconstant-load-2"))
  (when (and doc (stringp doc))
    ;; Documentation + localize bookkeeping; needs LISP-DATABASE-COLD.
    (cold-defer-guarded w (make-vsym "LANGUAGE-TOOLS" "DEFVAR-1-INTERNAL-1")
                        (list (make-vsym "LANGUAGE-TOOLS" "DEFVAR-1-INTERNAL-1")
                              (list (si-vsym "QUOTE") sym)
                              doc
                              (list (si-vsym "QUOTE") localize)))))

;;; ---------------- putprop ----------------

(defun cold-property-cell (w sym ind-tag ind-data ind-key)
  "The value cell for property IND on SYM's plist, creating the pair if
absent.  The host-side mirror in COLD-WORLD-PLISTS makes putprop replace
rather than shadow."
  (let* ((sym-key (fspec-key sym))
         (mirror (gethash sym-key (cold-world-plists w))))
    (or (cdr (assoc ind-key mirror :test #'equal))
        (let ((cell (cold-prepend-property
                     w (cold-vsym w sym) ind-tag ind-data
                     (tag 0 (cold-dtp w "NULL")) 0)))
          (push (cons ind-key cell) (gethash sym-key (cold-world-plists w)))
          cell))))

(defun cold-do-putprop (w sym value indicator &key (value-kind :object))
  "SYM and INDICATOR are decoded objects (the LMD path unwraps its QUOTEs
before calling); VALUE is an object or, with :FORM, a source form."
  (when (veval-p indicator)
    (setf indicator (quoted (veval-form indicator))))
  (when (veval-p sym)
    ;; e.g. ((**EVAL** :STRING) STRINGP TYPEP): keywords and quoted
    ;; symbols evaluate to themselves.
    (setf sym (quoted (veval-form sym))))
  (unless (vsym-p sym)
    (error "PUTPROP on non-symbol ~S" sym))
  (cold-note "putprop")
  (multiple-value-bind (it id) (cold-value-of-object w indicator)
    (unless it (error "Unevaluable PUTPROP indicator ~S" indicator))
    (multiple-value-bind (vt vd)
        (if (eq value-kind :form)
            (cold-eval-value w value)
            (cold-value-of-object w value))
      (let* ((ind-key (fspec-key indicator))
             (cell (cold-property-cell w sym it id ind-key))
             (value-form (if (veval-p value) (veval-form value) value)))
        (cond (vt (cw-set w cell vt vd))
              ((eq vd :patch)
               (cw-set w cell (tag 0 (cold-dtp w "NULL")) cell)
               (cold-note-patch w cell value-form))
              (t
               ;; Value exists only at boot: leave the property unbound-null
               ;; and defer the whole putprop (replaces in place at boot).
               (cw-set w cell (tag 0 (cold-dtp w "NULL")) cell)
               (cold-defer w (list (si-vsym "PUTPROP")
                                   (list (si-vsym "QUOTE") sym)
                                   value-form
                                   (list (si-vsym "QUOTE") indicator))
                           "deferred putprops")))))))

;;; ---------------- fdefine ----------------

(defun cold-do-fdefine (w fspec def &key (def-kind :object))
  "FDEFINE fspec def T (l-bin/load.lisp:584).  DEF is a vfun, a definition
object (macro cons etc.), or -- in the :FORM case (LOAD-MULTIPLE-DEFINITION
sub-forms) -- a source form such as (FUNCTION other) or (QUOTE (SPECIAL x))."
  (cold-note "fdefine")
  (let ((cell (cold-follow-cell w (cold-fdefinition-cell w fspec))))
    (flet ((store (tag data) (cw-set w cell tag data))
           (value (thing)
             (if (eq def-kind :form)
                 (cold-eval-value w thing)
                 (cold-value-of-object w thing))))
      (typecase def
        (vfun
         (let ((fn (cold-fun w def)))
           (store (tag 0 (cold-dtp w "COMPILED-FUNCTION")) fn)
           ;; FDEFINE marks the entry instruction CURRENT-DEFINITION-P
           ;; (bit 28) -- the one Q-diff M3c's oracle had to mask.
           (multiple-value-bind (etag edata) (cw-ref w fn)
             (cw-set w fn etag (logior edata (ash 1 28))))))
        (t
         (multiple-value-bind (tag data) (value def)
           (cond (tag (store tag data))
                 (t
                  ;; Forward reference through (FUNCTION x): retry at
                  ;; finalize, when x should be defined.
                  (cold-note "fdefine fixups")
                  (let ((pkg *cold-default-package*))
                    (push (lambda ()
                            (let ((*cold-default-package* pkg))
                              (multiple-value-bind (tag data) (value def)
                                (unless tag
                                  (error "Unresolved FDEFINE ~S <- ~S"
                                         fspec def))
                                (cw-set w cell tag data))))
                          (cold-world-fixups w))))))))))
  ;; The fdefs table already interned the cell under the fspec key.
  nil)

;;; ---------------- SET-TRAP-VECTOR-ENTRY ----------------

(defun cold-trap-base (w)
  (cold-address w "%TRAP-VECTOR-BASE"))

(defun cold-do-stve (w entry mode fspec pc-to-entry-p)
  "sys/iprim.lisp:61: store an even-pc Q whose CDR BITS are the trap mode.
ENTRY :CATCH-ALL (source entry T, legal only in the cold load) replaces
every slot still holding the skeleton's synthesized halt filler with the
real handler PC -- the 26:F8048DBB fill in the distribution trap page.
Comparing against the old filler instead of tracking explicit entries
makes the sweep order-independent: explicit vectors stored before it hold
real PCs and are skipped; ones deferred to fixups overwrite it later."
  (cold-note "set-trap-vector-entry")
  (unless (and (or (integerp entry) (eq entry :catch-all)) (integerp mode))
    (error "SET-TRAP-VECTOR-ENTRY with non-constant entry/mode: ~S ~S"
           entry mode))
  (let ((cell (cold-follow-cell w (cold-fdefinition-cell w fspec))))
    (flet ((attempt ()
             (multiple-value-bind (tag data) (cw-ref w cell)
               (when (= (tag-type tag) (cold-dtp w "COMPILED-FUNCTION"))
                 (let ((q-tag (logior (ash mode 6) (cold-dtp w "EVEN-PC")))
                       (pc (cold-fun-entry-pc w data
                                              :pc-to-entry-p pc-to-entry-p)))
                   (if (eq entry :catch-all)
                       (let ((base (cold-trap-base w))
                             (old-tag (tag 0 (cold-dtp w "EVEN-PC")))
                             (old-pc (cold-world-catch-all-pc w)))
                         (cold-note "trap catch-all fill")
                         (dotimes (i (layout-value (cold-world-layout w)
                                                   "%TRAP-VECTOR-LENGTH"))
                           (multiple-value-bind (st sd) (cw-ref w (+ base i))
                             (when (and (= st old-tag) (= sd old-pc))
                               (cw-set w (+ base i) q-tag pc))))
                         (setf (cold-world-catch-all-pc w) pc))
                       (cw-set w (+ (cold-trap-base w) entry) q-tag pc)))
                 t))))
      (unless (attempt)
        (cold-note "stve fixups")
        (push (lambda ()
                (unless (attempt)
                  (error "Trap vector ~S: ~S never became a compiled function"
                         entry fspec)))
              (cold-world-fixups w))))))

;;; ---------------- DECLARE-STORAGE-CATEGORY-LOAD ----------------

(defconstant +cold-wired-cell-table-size+ 832
  "Ground truth: the distribution world's *CURRENT-WIRED-SYMBOL-CELL-TABLE*
header is 43:C2018340 -- ART-Q, named-structure, leader 3, 832 elements.
The source's :WIRED descriptor says 200 (sys2/memory-cold.lisp:63), but
the generator sized its own table larger.")

(defun cold-wired-cell-table (w)
  "The FORWARDED-SYMBOL-CELL-TABLE the generator owns
(sys2/memory-cold.lisp:103).  Created on first use;
*CURRENT-WIRED-SYMBOL-CELL-TABLE* and *ALL-FORWARDED-SYMBOL-CELL-TABLES*
are set here -- BOOTSTRAP-FORWARD-SYMBOL-CELLS only creates the other
categories' tables at boot."
  (let ((table (cold-world-wired-cell-table w)))
    (if (plusp table)
        table
        (let* ((kw (lambda (n) (make-vsym "KEYWORD" n)))
               (bp (make-varray (list +cold-wired-cell-table-size+)
                                (list (funcall kw "TYPE")
                                      (make-vsym "SYSTEM" "ART-Q"))))
               (tbl (make-varray (list +cold-wired-cell-table-size+)
                                 (list (funcall kw "TYPE")
                                       (make-vsym "SYSTEM" "ART-Q")
                                       (funcall kw "LEADER-LENGTH") 3
                                       (funcall kw "FILL-POINTER") 0
                                       (funcall kw "NAMED-STRUCTURE-SYMBOL")
                                       (si-vsym "FORWARDED-SYMBOL-CELL-TABLE")
                                       (funcall kw "LEADER-LIST")
                                       (list nil nil bp)))))
          (let ((bp-vma (cold-array w bp "PERMANENT-STORAGE-AREA"))
                (tbl-vma (cold-array w tbl "WIRED-CONTROL-TABLES")))
            (declare (ignore bp-vma))
            (setf (cold-world-wired-cell-table w) tbl-vma
                  (cold-world-wired-cell-fill w) 0)
            (cold-set-symbol-value
             w (si-vsym "*CURRENT-WIRED-SYMBOL-CELL-TABLE*")
             (tag 0 (cold-dtp w "ARRAY")) tbl-vma)
            ;; (LIST table): one cdr-nil cell.
            (let ((cell (cold-alloc w "PERMANENT-STORAGE-AREA" 1)))
              (cw-set w cell
                      (logior (ash +cdr-nil+ 6) (cold-dtp w "ARRAY"))
                      tbl-vma)
              (cold-set-symbol-value
               w (si-vsym "*ALL-FORWARDED-SYMBOL-CELL-TABLES*")
               (tag 0 (cold-dtp w "LIST")) cell))
            tbl-vma)))))

(defun cold-forward-cell-into-wired-table (w cell-vma back-sym)
  "FORWARD-SYMBOL-CELL into the wired table: copy the cell Q to the next
table slot, forward the cell (preserving its cdr bits), and record the
back-pointer."
  (let* ((tbl (cold-wired-cell-table w))
         (index (cold-world-wired-cell-fill w))
         (slot (+ tbl 1 index)))
    (when (>= index +cold-wired-cell-table-size+)
      (error "Wired symbol cell table is full"))
    (multiple-value-bind (tag data) (cw-ref w cell-vma)
      (cw-set w slot (tag 0 (tag-type tag)) data)
      (cw-set w cell-vma
              (logior (logand tag #xC0) (cold-dtp w "ONE-Q-FORWARD"))
              slot))
    ;; fill pointer lives in leader 0 (header-1)
    (setf (cold-world-wired-cell-fill w) (1+ index))
    (cw-set w (- tbl 1) (tag 0 (cold-dtp w "FIXNUM")) (1+ index))
    ;; back-pointers array is leader 2
    (multiple-value-bind (bt bd) (cw-ref w (- tbl 3))
      (declare (ignore bt))
      (cw-set w (+ bd 1 index) (tag 0 (cold-dtp w "SYMBOL"))
              (cold-vsym w back-sym)))
    slot))

;;; ---------------- Keyword self-evaluation ----------------

(defconstant +cold-self-eval-table-size+ 11000
  "Initial :SELF-EVALUATING symbol-cell table size (the descriptor in
sys2/memory-cold.lisp:64).  The cold world interns ~1200 keywords; the
table has ample headroom and never needs the boot-time extension.")

(defun cold-self-eval-table (w)
  "The generator's :SELF-EVALUATING FORWARDED-SYMBOL-CELL-TABLE, in
CONSTANTS-AREA (MAKE-FORWARDED-SYMBOL-CELL-TABLE:112).  Leader length 3;
for :SELF-EVALUATING the back-pointers slot (leader 2) is the table itself
(memory-cold.lisp:120) -- the slot content already names each symbol, so
no separate back-pointer array is consed.  Created on first use.

Unlike the wired table this does NOT set *CURRENT-SELF-EVALUATING-SYMBOL-
TABLE* or *ALL-FORWARDED-SYMBOL-CELL-TABLES*: INITIALIZE-SELF-EVALUATING-
SYMBOL-TABLE re-creates *CURRENT-...* with a fresh table at first boot
(memory-cold.lisp:524) and BUILD-INITIAL-PACKAGES pushes that one.  This
cold table stays live through the keyword value-cell forwards that point
into it (it lives in static CONSTANTS-AREA)."
  (let ((table (cold-world-self-eval-table w)))
    (if (plusp table)
        table
        (let* ((kw (lambda (n) (make-vsym "KEYWORD" n)))
               (tbl (make-varray (list +cold-self-eval-table-size+)
                                 (list (funcall kw "TYPE")
                                       (make-vsym "SYSTEM" "ART-Q")
                                       (funcall kw "LEADER-LENGTH") 3
                                       (funcall kw "FILL-POINTER") 0
                                       (funcall kw "NAMED-STRUCTURE-SYMBOL")
                                       (si-vsym "FORWARDED-SYMBOL-CELL-TABLE")
                                       (funcall kw "LEADER-LIST")
                                       (list nil nil nil)))))
          (let ((tbl-vma (cold-array w tbl "CONSTANTS-AREA")))
            ;; back-pointers (leader 2, at header-3) = the table itself.
            (cw-set w (- tbl-vma 3) (tag 0 (cold-dtp w "ARRAY")) tbl-vma)
            (setf (cold-world-self-eval-table w) tbl-vma
                  (cold-world-self-eval-fill w) 0)
            tbl-vma)))))

(defun cold-forward-self-eval-symbol (w sym-vma)
  "Replicate FORWARD-SELF-EVALUATING-SYMBOL (memory-cold.lisp:529) at
generation time: store the symbol in the next self-eval table slot and
make its value cell a one-q-forward to that slot (preserving cdr bits).
SYMEVAL then follows the forward to the slot and reads back the symbol,
so the keyword self-evaluates."
  (let* ((tbl (cold-self-eval-table w))
         (index (cold-world-self-eval-fill w))
         (slot (+ tbl 1 index))
         (cell (+ sym-vma 1)))               ; value cell
    (when (>= index +cold-self-eval-table-size+)
      (error "Self-evaluating symbol cell table is full"))
    ;; The slot holds the symbol itself (PKG-NEW-KEYWORD-SYMBOL's SET SYM
    ;; SYM, then FORWARD stores SYMBOL into the table cell).
    (cw-set w slot (tag 0 (cold-dtp w "SYMBOL")) sym-vma)
    (multiple-value-bind (tag data) (cw-ref w cell)
      (declare (ignore data))
      (cw-set w cell
              (logior (logand tag #xC0) (cold-dtp w "ONE-Q-FORWARD"))
              slot))
    (setf (cold-world-self-eval-fill w) (1+ index))
    (cw-set w (- tbl 1) (tag 0 (cold-dtp w "FIXNUM")) (1+ index))  ; fill ptr
    slot))

(defun cold-forward-all-keywords (w)
  "Make every interned KEYWORD-package symbol self-evaluating.  Genera
interns keywords with PKG-NEW-KEYWORD-SYMBOL (package.lisp:1125), which
self-evaluates each one; the cold world pre-interns keywords, so the
generator must do the equivalent before BUILD-INITIAL-PACKAGES EVALs its
DEFPACKAGE-INTERNAL forms.  Returns the number forwarded.  (NIL and T are
folded to architectural blocks, not interned here, and are forwarded at
boot by INITIALIZE-SELF-EVALUATING-SYMBOL-TABLE.)"
  (let ((keywords nil))
    (maphash (lambda (key vma)
               (when (equal (cdr key) "KEYWORD")
                 (push vma keywords)))
             (cold-world-symbols w))
    ;; Deterministic order: ascending symbol vma (intern order is
    ;; hash-traversal order, which SBCL does not promise across runs).
    (dolist (vma (sort keywords #'<))
      (cold-forward-self-eval-symbol w vma))
    (length keywords)))

(defun cold-do-dscl (w ref-type sym category)
  "DECLARE-STORAGE-CATEGORY-LOAD (sys2/storage-categories.lisp:553).
Wired cells are forwarded by the generator; safeguarded ones only get the
declaration property -- BOOTSTRAP-FORWARD-SYMBOL-CELLS pass 0 forwards
them at first boot (sys2/memory-cold.lisp:279)."
  (cold-note "declare-storage-category-load")
  (let* ((ref (if (vsym-p ref-type) (vsym-name ref-type)
                  (vsym-name (quoted ref-type))))
         (cat (if (vsym-p category) (vsym-name category)
                  (vsym-name (quoted category))))
         (sym (quoted sym))
         (prop (cond ((string= ref "VARIABLE") "VARIABLE-STORAGE-CATEGORY")
                     ((string= ref "FUNCTION-CELL")
                      "FUNCTION-CELL-STORAGE-CATEGORY")
                     (t (error "DECLARE-STORAGE-CATEGORY-LOAD ~A?" ref)))))
    (unless (vsym-p sym)
      (error "DECLARE-STORAGE-CATEGORY-LOAD of ~S" sym))
    (when (string= cat "UNSAFEGUARDED")
      ;; remprop of a property the cold plist never carries.
      (return-from cold-do-dscl nil))
    ;; (SETF (GET sym 'prop) :category)
    (multiple-value-bind (it id) (cold-symbol-ref w (si-vsym prop))
      (let ((cell (cold-property-cell w sym it id
                                      (fspec-key (si-vsym prop)))))
        (multiple-value-bind (ct cd)
            (cold-symbol-ref w (make-vsym "KEYWORD" cat))
          (cw-set w cell ct cd))))
    (when (string= cat "WIRED")
      (let ((cell (1+ (cold-vsym w sym))))    ; value cell
        (when (string= ref "FUNCTION-CELL")
          (setf cell (+ (cold-vsym w sym) 2)))
        (multiple-value-bind (tag data) (cw-ref w cell)
          (declare (ignore data))
          (if (= (tag-type tag) (cold-dtp w "ONE-Q-FORWARD"))
              (cold-note "dscl already forwarded")
              (cold-forward-cell-into-wired-table w cell sym)))))))

;;; ---------------- ASET ----------------

(defparameter *cold-aset-autocreate*
  '(("*INTERNAL-READABLE-REGISTER-MAP*" . 1024)
    ("*INTERNAL-WRITABLE-REGISTER-MAP*" . 1024))
  "Arrays the generator itself must create: DEFINE-INTERNAL-REGISTERS
(i-compiler/i-sysdef-support.lisp:394) ASETs into them from sysdf1, but
their DEFVAR-SAFEGUARDED (sys/ldata.lisp:210) carries no initializer and
no cold file makes them.  Ground truth (Genera-8-5.vlod) holds ART-Q
#o2000-element arrays, header 43:C0000400.")

(defun cold-do-aset (w value array-form index)
  "(ASET value array-symbol index) on a boxed 1-D ART-Q array."
  (cold-note "aset")
  (unless (and (vsym-p array-form) (integerp index))
    (error "Unsupported ASET target ~S[~S]" array-form index))
  (let ((auto (assoc (vsym-name array-form) *cold-aset-autocreate*
                     :test #'string=)))
    (when (and auto (not (nth-value 2 (cold-symbol-value-q w array-form))))
      (let ((arr (make-varray (list (cdr auto))
                              (list (make-vsym "KEYWORD" "TYPE")
                                    (make-vsym "SYSTEM" "ART-Q")))))
        (cold-set-symbol-value
         w array-form (tag 0 (cold-dtp w "ARRAY"))
         (cold-array w arr "SAFEGUARDED-OBJECTS-AREA")))))
  (multiple-value-bind (at ad boundp) (cold-symbol-value-q w array-form)
    (unless (and boundp (= (tag-type at) (cold-dtp w "ARRAY")))
      (error "ASET into ~S which is not a bound array" array-form))
    (multiple-value-bind (ht hd) (cw-ref w ad)
      (declare (ignore ht))
      (let ((packing (ldb (byte 3 27) hd))
            (len (ldb (byte 15 0) hd)))
        (unless (and (zerop packing) (not (logbitp 23 hd)) (< index len))
          (error "ASET ~S[~D]: unsupported header #x~8,'0X"
                 array-form index hd))
        (multiple-value-bind (vt vd) (cold-eval-value w value)
          (unless vt (error "Unevaluable ASET value ~S" value))
          (cw-set w (+ ad 1 index) vt vd))))))

;;; ---------------- Misc native handlers ----------------

(defun cold-do-initialize-pointer-type-p-array (w)
  "(SETQ *POINTER-TYPE-P* (%P-CONTENTS-OFFSET #'%POINTER-TYPE-P 1)):
the allocator's type-dispatch table is the function's first constant
(sys/i-allocate.lisp:372).  Needed before any consing, so it cannot wait
for the deferred forms."
  (cold-note "initialize-pointer-type-p-array")
  (let ((cell (cold-follow-cell
               w (cold-fdefinition-cell w (si-vsym "%POINTER-TYPE-P")))))
    (multiple-value-bind (tag data) (cw-ref w cell)
      (unless (= (tag-type tag) (cold-dtp w "COMPILED-FUNCTION"))
        (error "%POINTER-TYPE-P not yet defined"))
      (multiple-value-bind (qt qd) (cw-ref w (1+ data))
        (cold-set-symbol-value w (si-vsym "*POINTER-TYPE-P*")
                               (tag 0 (tag-type qt)) qd)))))

;;; ---------------- The routing table ----------------

(defparameter *cold-defer-heads*
  '("PROCLAIM" "RECORD-SOURCE-FILE-NAME" "RECORD-DEFINITION-SOURCE-FILE"
    "REMEMBER-VARIABLE-BINDING"
    "DEFMACRO-SET-INDENTATION-FOR-ZWEI" "DEFMACRO-CLEAR-INDENTATION-FOR-ZWEI"
    "ADD-INITIALIZATION" "REDEFINE-GC-OPTIMIZATION"
    "DEFINE-SETF-PROPERTY" "DEFINE-LOCF-PROPERTY"
    ;; iofns.lisp:1088 defines it and its own load forms call it (the CL
    ;; WRITE keyword registry); everything its body touches is cold.
    "ADD-WRITE-KEYWORD"
    "START-DEFSTRUCT-DEFINITION" "FINISH-DEFSTRUCT-DEFINITION"
    "INITIALIZE-RESOURCE" "REDEFINE-FORMAT-DIRECTIVE"
    "DEFGENERIC-INTERNAL"
    "INITIALIZE-READTABLE-SYNTAX-AND-NAME" "FILL-ASCII-TRANSLATION-TABLES"
    "SET-SYNTAX-#-MACRO-CHAR" "NREMPROP" "BLOCK")
  "Heads whose owners are cold files or cold-load stubs: safe to evaluate
verbatim at first boot, before the banner.  PROCLAIM lives in
SYS:SYS;LISP-DATABASE-COLD -- which the distribution cold load contained
and the M2 file list must gain (see plan).")

(defparameter *cold-guarded-heads*
  '("DEFFLAVOR-INTERNAL" "NOTE-SOLITARY-METHOD"
    "COMPILE-FLAVOR-METHODS-LOAD-TIME" "ADD-OPTIMIZER-INTERNAL"
    "ADD-TRANSFORMER"
    "LOOP-ADD-PATH" "ADD-IO-VARIABLE" "ADD-IE-COMMAND"
    "ADD-PROMPT-AND-READ-KEYWORD"
    ;; CLI:INTERNAL deftype records (octet-structure): owner is CLCP.
    "START-DEFTYPE-DEFINITION" "FINISH-DEFTYPE-DEFINITION")
  "Heads owned by QLD-era systems (flavors, compiler, LOOP, CLCP io):
deferred behind (IF (FBOUNDP ...)) so first boot skips them silently.
Their registrations are re-established when the owning system loads --
KNOWN GAP for anything QLD does not reload; tracked in the plan.")

(defparameter *cold-noop-heads*
  '("MAKE-OBSOLETE-1" "MAKE-MESSAGE-OBSOLETE" "DELETE-TRANSFORMER-INTERNAL")
  "Development-environment style-checker bookkeeping; their functions are
compiler-side and have no cold definition or boot effect.")

(defun cold-eval-toplevel (w form)
  (let ((head (form-head-name form))
        (args (and (consp form) (rest form))))
    (cond
      ((null head)
       (error "Unhandled top-level form ~S" form))
      ((string= head "DEFVAR-1")
       (cold-do-defvar w :defvar (first args) (second args) (cdr args)
                       (third args) (fourth args) :value-kind :form))
      ((string= head "DEFCONST-1")
       (cold-do-defvar w :defconst (first args) (second args) t
                       (third args) nil :value-kind :form))
      ((string= head "DEFCONSTANT-1")
       (cold-do-defvar w :defconstant (first args) (second args) t
                       (third args) nil :value-kind :form))
      ((string= head "SET-TRAP-VECTOR-ENTRY")
       (let ((entry (if (vsym-named-p (first args) "T")
                        :catch-all
                        (first args)))
             (mode (second args))
             (fspec (quoted (third args)))
             (pc-to-entry (and (fourth args)
                               (not (vsym-named-p (fourth args) "NIL")))))
         (cold-do-stve w entry mode fspec pc-to-entry)))
      ((string= head "DECLARE-STORAGE-CATEGORY-LOAD")
       (cold-do-dscl w (first args) (second args) (third args)))
      ((string= head "ASET")
       (cold-do-aset w (first args) (second args) (third args)))
      ((string= head "FDEFINE")
       ;; LOAD-MULTIPLE-DEFINITION sub-form: args are source forms.
       (cold-do-fdefine w (quoted (first args)) (second args)
                        :def-kind :form))
      ((string= head "PUTPROP")
       (cold-do-putprop w (quoted (first args)) (second args)
                        (quoted (third args)) :value-kind :form))
      ((string= head "SETQ")
       (multiple-value-bind (tag data) (cold-eval-value w (second args))
         (if tag
             (progn (cold-note "setq")
                    (cold-set-symbol-value w (first args) tag data))
             (cold-defer w form "deferred setqs"))))
      ((string= head "PROGN")
       (mapc (lambda (f) (cold-eval-toplevel w f)) args))
      ((or (string= head "OR") (string= head "IF"))
       ;; The (OR (MEMBER-FAST ...) (SETQ ...)) registration idiom; its
       ;; functions are CLCP-side, so it must run natively.
       (multiple-value-bind (tag data) (cold-eval-value w (first args))
         (cond ((null tag) (cold-defer w form "deferred conditionals"))
               ((string= head "OR")
                (when (cold-q-nil-p w tag data)
                  (dolist (f (rest args))
                    (multiple-value-bind (tag2 data2) (cold-eval-value w f)
                      (declare (ignore data2))
                      (unless tag2
                        (error "Unevaluable OR arm ~S" f)))))
                (cold-note "or"))
               (t                        ; IF
                (let ((arm (if (cold-q-nil-p w tag data)
                               (third args)
                               (second args))))
                  (when arm
                    (multiple-value-bind (tag2 data2) (cold-eval-value w arm)
                      (declare (ignore data2))
                      (unless tag2 (error "Unevaluable IF arm ~S" arm))))
                  (cold-note "if"))))))
      ((string= head "LOAD-MULTIPLE-DEFINITION")
       ;; (LMD 'name 'type 'body env) evaluates the body forms in order
       ;; (sys/eval.lisp:2161); source-file recording is deferred.
       (cold-note "load-multiple-definition")
       (let ((name (quoted (first args)))
             (type (quoted (second args)))
             (body (quoted (third args))))
         (cold-defer w (list (si-vsym "RECORD-DEFINITION-SOURCE-FILE")
                             (list (si-vsym "QUOTE") name)
                             (list (si-vsym "QUOTE") type))
                     "record-definition-source-file")
         (dolist (sub body)
           (cold-eval-toplevel w sub))))
      ((string= head "DEFINE-MAGIC-LOCATIONS-1")
       ;; Forwards the variables' value/function cells into the comm block
       ;; NOW (sysdf1 load time), like the original generator: later SETQs
       ;; and FDEFINEs write through the forwards, and later DSCL :WIRED
       ;; declarations see the cell already forwarded and skip it.
       (cold-note "define-magic-locations-1")
       (let ((parsed (mapcar #'quoted args)))
         (push parsed (cold-world-magic w))
         (cold-do-define-magic-locations w parsed)))
      ((string= head "INITIALIZE-POINTER-TYPE-P-ARRAY")
       (cold-do-initialize-pointer-type-p-array w))
      ((or (string= head "LINK-SYMBOL-VALUE-CELLS")
           (string= head "LINK-SYMBOL-FUNCTION-CELLS"))
       ;; clcp/permanent-links.lisp: "These functions are actually
       ;; simulated by the cold load generator."  The simulation is a
       ;; RECORD, not the forwarding itself: the entries become
       ;; SI:*LINKED-SYMBOL-CELLS*, which BOOTSTRAP-FORWARD-SYMBOL-CELLS
       ;; consumes at first boot (sys2/memory-cold.lisp:286-292).
       (cold-note (string-downcase head))
       (let ((from (quoted (first args)))
             (to (quoted (second args))))
         (unless (and (vsym-p from) (vsym-p to))
           (error "Unsupported ~A arguments ~S" head args))
         (push (list from to
                     (if (string= head "LINK-SYMBOL-VALUE-CELLS")
                         (make-vsym "KEYWORD" "VARIABLE")
                         (make-vsym "KEYWORD" "FUNCTION")))
               (cold-world-linked-cells w))))
      ((member head *cold-noop-heads* :test #'string=)
       (cold-note (format nil "noop ~A" head)))
      ((member head *cold-defer-heads* :test #'string=)
       (cold-defer w form (format nil "defer ~A" head)))
      ((member head *cold-guarded-heads* :test #'string=)
       (cold-defer-guarded w (first form) form))
      (t (error "Unknown top-level head ~A in ~S" head form)))))

;;; ---------------- Event + file drivers ----------------

(defun cold-load-event (w op value)
  (declare (ignorable op))
  (etypecase value
    (veval (cold-eval-toplevel w (veval-form value)))
    (vop
     (let ((args (vop-args value)))
       (ecase (vop-op value)
         (:fdefine (cold-do-fdefine w (first args) (second args)))
         (:setq
          (multiple-value-bind (tag data)
              (cold-value-of-object w (second args))
            (if tag
                (progn (cold-note "setq")
                       (cold-set-symbol-value w (first args) tag data))
                (cold-defer w (list (si-vsym "SET")
                                    (list (si-vsym "QUOTE") (first args))
                                    (let ((v (second args)))
                                      (if (veval-p v) (veval-form v) v)))
                            "deferred setqs"))))
         (:putprop (cold-do-putprop w (first args) (second args)
                                    (third args)))
         ;; The value operand of the DEFVAR/DEFCONST bin ops is the
         ;; SOURCE FORM (DEFVAR-1 semantics -- evaluated lazily by the
         ;; loader), not a quoted object: constants coincide either way,
         ;; but (QUOTE X) must store X and calls like (MAKE-AREA ...)
         ;; must evaluate (M3h boot-11: *WIRED-CONSOLE-AREA* held its
         ;; own make-area form; dist holds area number 19).
         (:defconst (cold-do-defvar w :defconst (first args) (second args)
                                    (> (length args) 1) (third args) nil
                                    :value-kind :form))
         (:defvar (cold-do-defvar w :defvar (first args) (second args)
                                  (> (length args) 1)
                                  (third args) (fourth args)
                                  :value-kind :form))
         (:attribute-list nil))))
    ;; initialize-array events already mutated their varray during decode.
    (symbol nil)
    (t nil)))

(defun cold-load-vbin (w path)
  (let* ((vbin (read-vbin path))
         (pkg (vbin-attribute vbin "PACKAGE"))
         (*cold-default-package*
           (canonical-package-string
            (etypecase pkg
              (vsym (vsym-name pkg))
              (string pkg)
              ;; e.g. (:AUDIO :USE (:SYSTEM :GLOBAL)): in-line package spec.
              (cons (vsym-name (first pkg)))
              (null *cold-default-package*)))))
    (loop for event in (vbin-file-events vbin)
          for n from 0
          do (handler-case (cold-load-event w (car event) (cdr event))
               (error (e)
                 (error "~A event ~D (~A): ~A"
                        *cold-eval-file* n (car event) e))))))

(defun cold-load-cold-set (w)
  "Load all cold-set vbins in order, then run fixups to quiescence.
Returns the number of fixups that never resolved (their errors are
collected into *COLD-EVAL-STATS* under \"fixup failures\")."
  (with-cold-materializer (w)
    (let ((*cold-load-time-eval* #'cold-operand-eval))
      (dolist (spec *cold-load-order*)
        (let ((*cold-eval-file* spec))
          (cold-load-vbin w (sys-pathname spec))))
      ;; Late-bound references: retry until quiescent.
      (let ((failures 0))
        (loop repeat 4
              for pending = (shiftf (cold-world-fixups w) nil)
              while pending
              do (dolist (fixup pending)
                   (handler-case (funcall fixup)
                     (error (e)
                       (declare (ignore e))
                       (push fixup (cold-world-fixups w))))))
        (dolist (fixup (cold-world-fixups w))
          (handler-case (funcall fixup)
            (error (e)
              (incf failures)
              (cold-note "fixup failures")
              (when (<= failures 5)
                (cold-note (format nil "fixup: ~A" e))))))
        failures))))
