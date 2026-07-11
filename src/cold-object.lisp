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

;;; Heap area regions (quantum-aligned; the storage tables make these the
;;; world's address-space policy).  SAFEGUARDED-OBJECTS-AREA is region 2 at
;;; #xF0000000 (architectural, made by the skeleton); ground truth puts the
;;; unwired heap in the #x80000000 zone.
(defparameter *cold-heap-regions*
  '(("SYMBOL-AREA"             #x80100000 #x100000)
    ("PNAME-AREA"              #x80300000 #x100000)
    ("PROPERTY-LIST-AREA"      #x80500000 #x100000)
    ("PERMANENT-STORAGE-AREA"  #x80700000 #x400000)
    ("WORKING-STORAGE-AREA"    #x80C00000 #x100000)
    ("COMPILED-FUNCTION-AREA"  #x81000000 #x800000)
    ("DEBUG-INFO-AREA"         #x81900000 #x400000)
    ("CONSTANTS-AREA"          #x81E00000 #x100000)))

;;; LIST-representation companions for the areas that receive generator
;;; conses (COLD-ALLOC rep :LIST), in the address gaps between the
;;; structure regions.  An area holding both representations region-by-
;;; region is exactly the distribution's shape (its PERMANENT-STORAGE-AREA
;;; carries 02880048 LIST next to 02880049 STRUCTURE regions); a cons in
;;; a STRUCTURE region cannot be RPLACD'd -- REDEFINE-GC-OPTIMIZATION-1's
;;; boot-time PUSHNEW onto the dumped *IMMEDIATE-GC-MODE-OPTIMIZATION-
;;; ALIST* sublists trapped in RPLACD-ESCAPE (M3h boot 34).
(defparameter *cold-heap-list-regions*
  '(("PROPERTY-LIST-AREA"      #x80600000 #x100000)
    ("PERMANENT-STORAGE-AREA"  #x80B00000 #x100000)
    ("WORKING-STORAGE-AREA"    #x80D00000 #x100000)
    ;; The forged MAKE-INSTANCE generic-function object's home
    ;; (cold-build-make-instance-generic): DEFGENERIC-INTERNAL allocates
    ;; GF objects in FLAVOR:*FLAVOR-STATIC-AREA* (defgeneric.lisp:634;
    ;; ":GC :STATIC :REPRESENTATION :LIST", flavor/global.lisp:113), and
    ;; the dist's cold MAKE-INSTANCE GF at #x8800807C sits in that
    ;; area's (26) LIST region.  A boot-created area may own a
    ;; generator region: it is registered like any other in the region
    ;; tables and threaded from the area's REGION-LIST row (M3h boot 36).
    ("*FLAVOR-STATIC-AREA*"    #x80E00000 #x100000)))

(defun cold-add-heap-regions (w)
  (loop for (name origin length) in *cold-heap-regions*
        do (cold-add-region w name origin length))
  ;; Reserved address space for boot-time wired allocation, in the
  ;; distribution's region order (14/15/16): initialize-disk resets
  ;; WIRED-DYNAMIC-AREA's free pointer and allocates disk structures
  ;; there; build-address-space-map's create-dynamic-space allocates in
  ;; PAGE-TABLE-AREA after initialize-storage-globals resets it; the GC
  ;; grows its tables in GC-TABLE-AREA.  Each region's identity reaches
  ;; the resetters through %<AREA>-REGION{,-ORIGIN,-LENGTH} wired
  ;; variables, which cold-machinery stamps.
  (cold-add-region w "WIRED-DYNAMIC-AREA" #xF0030000 #x10000)
  (cold-add-region w "PAGE-TABLE-AREA" #xF0040000 #x200000 :rep :list)
  (cold-add-region w "GC-TABLE-AREA" #xF0240000 #x80000 :rep :list)
  ;; The LIST regions come last so the reserved trio keeps the
  ;; distribution's region numbers 14/15/16 (check-reserved-regions).
  (loop for (name origin length) in *cold-heap-list-regions*
        do (cold-add-region w name origin length :rep :list)))

;;; EQ identity for materialized host objects (conses, varrays, vfuns).
(defvar *cold-object-vmas*)

(defvar *cold-cca-base* nil
  "CCA address of the function being materialized; vembed constants are
CCA-relative.")

(defvar *cold-load-time-eval*
  (lambda (w form)
    (declare (ignore w))
    (error "Load-time eval ~S needs the mini-eval (M3d)" form))
  "Hook: (**EVAL** form) reached as an operand; returns the value's
(values tag data).  Bound to the mini-eval by the driver.")

(defvar *cold-eval-patch-form* nil
  "Set by the mini-eval when an operand's value only exists at run time:
the Q-store site that consumed the placeholder records a first-boot patch
(cold-note-patch) and clears this.")

(defun cold-note-patch (w vma form)
  "Queue a first-boot %P-STORE-CONTENTS of (eval FORM) into VMA."
  (push (list vma *cold-default-package* form) (cold-world-patches w)))

(defparameter *cold-guarded-patch-heads*
  '("LOAD-TIME-FIND-FLAVOR")
  "Patch value heads that are warm-only: the finalize spine wraps these
patches in (IF (FBOUNDP 'head) ...) so first boot skips them silently,
and the boot-safety audit exempts them.  LOAD-TIME-FIND-FLAVOR lives in
flavor/make.lisp:976 -- NOT cold (the MAKE-INSTANCE-COLD bridge exists
because instantiation is warm), dist fcell forwards into the QLD band.
Its one patch fills a defflavor constructor constant
\(unix-translating-streams' 8BIT-BINARY-STREAM-...); constructors never
run pre-banner (MAKE-INSTANCE = the -COLD marker path), and make.lisp's
own comment says the constructor 'will be redefined soon, by
COMPILE-FLAVOR-METHODS-LOAD-TIME'.  KNOWN GAP: the Q stays NIL until
warm CFM redefines the constructor (M3h boot 28).")

;;; The vembed dtp-code -> type name (l-bin/defs.lisp embedded constants).
(defparameter *cold-embed-types*
  #("LIST" "LEXICAL-CLOSURE" "DYNAMIC-CLOSURE" "DOUBLE-FLOAT"
    "BIG-RATIO" "COMPLEX" "COMPILED-FUNCTION" "LOCATIVE"))

;;; Character-set slots (%%CHAR-CHAR-SET, byte 8 8 of the char word).
;;; Slots are assigned warm on first use (character-sets.lisp
;;; ASSIGN-OFFSETS: standard = 0 and mouse = 1 asserted, the rest
;;; first-free), so a cold character must bake in the slot the finished
;;; world will assign.  Keyboard = 2 is ground truth: all seven cold-set
;;; Keyboard characters (input-editor command registrations) appear in
;;; the distribution world with charset byte 2 (e.g. Cut = 23:00000200,
;;; c-Back-Scroll = 23:10000203, Find = 23:00000268).
(defparameter *cold-character-set-slots*
  '(("Standard" . 0) ("Mouse" . 1) ("Keyboard" . 2)))

(defun cold-charset-slot (vchar)
  (let* ((cs (vchar-charset vchar))
         (name (let ((n (vcharset-name cs)))
                 (if (vsym-p n) (vsym-name n) n))))
    (or (cdr (assoc name *cold-character-set-slots* :test #'string-equal))
        (error "Character set ~A has no known cold slot (see ~
*COLD-CHARACTER-SET-SLOTS*)" name))))


(defmacro with-cold-materializer ((w) &body body)
  "Establish the host-object identity table.  Re-entrant: a nested use
joins the enclosing scope, so a driver can wrap load + machinery +
finalize in ONE scope and deferred forms keep aliasing the structure the
load already materialized (vbin table slots share host conses)."
  (declare (ignore w))
  `(let ((*cold-object-vmas* (if (boundp '*cold-object-vmas*)
                                 *cold-object-vmas*
                                 (make-hash-table :test #'eq))))
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

;;; Package nicknames (sys/pkgdcl.lisp DEFPACKAGE :NICKNAMES clauses).
;;; File attribute lists and refnames use nicknames ("SI", "CLI"); folding
;;; them to the full name keeps the (pname . package) intern table from
;;; splitting one runtime symbol into duplicates.
(defparameter *cold-package-nicknames*
  '(("" . "KEYWORD")                    ; pkgdcl: (:NICKNAMES "")
    ("CL" . "LISP") ("COMMON-LISP" . "LISP") ("COMMON-LISP-GLOBAL" . "LISP")
    ("SCL" . "SYMBOLICS-COMMON-LISP")
    ("SYS" . "SYSTEM") ("ZETALISP-SYSTEM" . "SYSTEM")
    ("COMMON-LISP-SYSTEM" . "SYSTEM") ("CL-SYS" . "SYSTEM")
    ("ZL" . "GLOBAL") ("ZETALISP" . "GLOBAL") ("ZETALISP-GLOBAL" . "GLOBAL")
    ("ZL-USER" . "ZETALISP-USER")
    ("SCT" . "SYSTEM-CONSTRUCTION-TOOL")
    ("SI" . "SYSTEM-INTERNALS")
    ("DBG" . "DEBUGGER")
    ("FS" . "FILE-SYSTEM")
    ("NETI" . "NETWORK-INTERNALS")
    ("LT" . "LANGUAGE-TOOLS")
    ("CLI" . "COMMON-LISP-INTERNALS")
    ("CL-USER" . "COMMON-LISP-USER")
    ("FCLI" . "FUTURE-COMMON-LISP-INTERNALS")
    ("FCL-USER" . "FUTURE-COMMON-LISP-USER")
    ("SU" . "SERVER-UTILITIES")
    ("DW" . "DYNAMIC-WINDOWS")
    ("CP" . "COMMAND-PROCESSOR")
    ("NET" . "NETWORK")))

(defun canonical-package-string (name)
  (or (cdr (assoc name *cold-package-nicknames* :test #'string=)) name))

(defun canonical-package-name (package)
  "Fold a vsym/vpackage package spec to the name string stored in the cold
symbol's package cell (PKG-FIND-PACKAGE resolves it at first boot).
Refname lists fold to their last package string: the INTERNAL marker only
selects plain interning (l-bin/load.lisp GET-PACKAGE-FROM-REFNAME-LIST);
dotted (\"syntax\" . \"name\") pairs fold to the name."
  (etypecase package
    ((eql :default) (canonical-package-string *cold-default-package*))
    ((eql :uninterned) nil)
    (string (canonical-package-string package))
    (vpackage (canonical-package-name (vpackage-spec package)))
    (cons
     (canonical-package-string
      (if (stringp (cdr package))
          (cdr package)
          (let ((name nil))
            (dolist (p package (or name (error "Empty refname list")))
              (when (stringp p) (setf name p)))))))))

;;; Symbol-home resolution.  A dumped plain symbol means "accessible from
;;; the file package" -- possibly inherited -- so interning by file package
;;; would split one runtime symbol into per-package duplicates with
;;; disconnected value/function cells.  The distribution world's symbol
;;; blocks provide the true pname -> home mapping (world-symbol-homes);
;;; when it is loaded, references resolve through it.
(defvar *cold-symbol-homes* nil)     ; pname -> list of home package names
(defvar *cold-package-aliases* nil)  ; package name/nickname -> primary name
(defvar *cold-package-uses* nil)     ; pkgdcl: primary name -> :USE list
(defvar *cold-package-imports* nil)  ; pkgdcl: (primary . pname) -> source pkg
(defvar *cold-package-exports* nil)  ; pkgdcl: pname -> list of exporting pkgs
(defvar *cold-package-shadows* nil)  ; pkgdcl: (primary . pname) -> T
(defvar *cold-package-external-only* nil) ; pkgdcl: primary -> T (locked; interns external)
(defvar *cold-package-defs* nil)     ; pkgdcl: ordered per-package clause plists

(defparameter *cold-home-priority*
  '("GLOBAL" "LISP" "SYSTEM" "SYMBOLICS-COMMON-LISP" "SYSTEM-INTERNALS"
    "COMMON-LISP-INTERNALS" "LANGUAGE-TOOLS" "STORAGE" "KEYWORD")
  "Tie-break order for pnames with several homes when the referencing
package is not itself one of them: the widely-inherited packages first.")

(defun cold-package-primary (name)
  (or (and *cold-package-aliases* (gethash name *cold-package-aliases*))
      name))

(defun cold-resolve-through-imports (pname ctx homes)
  "Resolve PNAME's home from CTX through the pkgdcl graph: a package with
an :IMPORT-FROM/:IMPORT entry for PNAME means the source package's symbol;
a package that is itself a home of PNAME claims it; otherwise the search
descends the :USE list breadth-first.  NIL when the graph has no answer
\(the caller falls back to the priority heuristic).  This is what keeps
Genera's dialect-split symbols split: SCL and FCL import FORMAT from LISP
\(pkgdcl.lisp:3172, \"string incompatibility\") while GLOBAL exports its
own, so CLCP code reaches LISP:FORMAT and ZL code GLOBAL:FORMAT.
Coalescing them by flat priority let iofns' CL wrapper occupy the one
FORMAT and suppress the FBOUNDP-guarded FORMAT-COLD-LOAD boot stub
\(cold-load.lisp:530) -- M3h boot 15."
  (when (and *cold-package-uses* *cold-package-imports*)
    (let ((queue (list (cold-package-primary ctx)))
          (seen nil))
      (loop while queue
            for pkg = (pop queue)
            unless (member pkg seen :test #'string=)
              do (push pkg seen)
                 (let ((src (gethash (cons pkg pname)
                                     *cold-package-imports*)))
                   (cond (src
                          (setf queue
                                (nconc queue
                                       (list (cold-package-primary src)))))
                         ((member pkg homes :test #'string=)
                          (return pkg))
                         (t
                          (setf queue
                                (nconc queue
                                       (mapcar #'cold-package-primary
                                               (gethash pkg
                                                        *cold-package-uses*)))))))
            finally (return nil)))))

(defun cold-find-visible-home (pname pkg homes &optional seen)
  "Emulate CL:FIND-SYMBOL's one-level visibility from PKG
\(PKG-FIND-SYMBOL, package.lisp:906): present symbols first -- an
:IMPORT-FROM entry means the present symbol IS the source package's,
and PKG being one of PNAME's dist homes means PNAME is interned there
-- then the EXTERNALs of the DIRECT :USE list only.  A used package
provides PNAME if its pkgdcl :EXPORT lists it, or if the package is
\(:EXTERNAL-ONLY T) (package.lisp:512: every present symbol is
external -- SCL's ~730 :IMPORT-FROM LISP symbols are all externals)
and PNAME is present in it.  The provider's symbol is then resolved
recursively (its own import entry carries the identity onward).
COLD-RESOLVE-THROUGH-IMPORTS' transitive BFS descends :USE edges
without any export evidence, so from RPC (:USE SYSTEM SCL) it reached
GLOBAL:SETF through SYSTEM (:USE GLOBAL) -- a symbol FIND-SYMBOL in
RPC never sees, and the one SETF that never gets fspec.lisp:1752's
DEFINE-DERIVED-FUNCTION-TYPE DEFPROPs, so pass 1's FDEFINEDP on
\(SETF BYTE-SWAPPED-LOCATIVE-REF-32) failed validation and called
warm-only DBG:CHECK-ARG-1 (M3h boot 27).  NIL when no provider
answers; the caller falls back to the BFS + priority heuristics."
  (let ((pkg (cold-package-primary pkg)))
    (unless (member pkg seen :test #'string=)
      (push pkg seen)
      (let ((src (gethash (cons pkg pname) *cold-package-imports*)))
        (cond
          (src
           (or (cold-find-visible-home pname src homes seen)
               (cold-package-primary src)))
          ((member pkg homes :test #'string=) pkg)
          (t
           (loop for u in (gethash pkg *cold-package-uses*)
                 for uu = (cold-package-primary u)
                 when (or (member uu (gethash pname *cold-package-exports*)
                                  :test #'string=)
                          (and (gethash uu *cold-package-external-only*)
                               (or (gethash (cons uu pname)
                                            *cold-package-imports*)
                                   (member uu homes :test #'string=))))
                   return (or (cold-find-visible-home pname uu homes seen)
                              uu))))))))

(defun cold-declared-package-p (name)
  "Does PKGDCL declare NAME, so BUILD-INITIAL-PACKAGES will create it at
first boot?  With no graph loaded every name passes."
  (or (null *cold-package-uses*)
      (nth-value 1 (gethash name *cold-package-uses*))))

(defun cold-use-provider-of (pname pkg)
  "The first directly-:USEd package whose PKGDCL :EXPORT lists PNAME.
This is CL:FIND-SYMBOL's inheritance reach (package.lisp:906): present
symbols, then the EXTERNALs of the DIRECT use list only -- never
transitive.  Deeper chains resolve stepwise, and only through packages
that re-export the pname themselves."
  (when (and *cold-package-uses* *cold-package-exports*)
    (let ((exporters (gethash pname *cold-package-exports*)))
      (when exporters
        (loop for q in (gethash pkg *cold-package-uses*)
              for qq = (cold-package-primary q)
              when (member qq exporters :test #'string=)
                return qq)))))

(defun cold-adjust-home-for-exports (pname home)
  "Re-home PNAME from HOME (the dist's warm state -- ADJUST-HOME-PACKAGES,
package.lisp:2474, re-homes CLOS/CONDITIONS externals and an explicit
SCL/FCL list after the boot) onto the package whose symbol PNAME must
already be at first boot.  Two relations, followed to a fixed point:
\(a) HOME has a pkgdcl :IMPORT-FROM entry for PNAME -- its symbol IS the
source package's; a cold symbol homed at HOME instead makes the import
pass signal NAME-CONFLICT-IN-IMPORT against the fresh symbol INTERN
creates in the source (*PRINT-READABLY*: dist homes it in SCL, but SCL
imports it from FUTURE-COMMON-LISP, pkgdcl.lisp:3620 -- M3h boot 20).
\(b) a package HOME directly :USEs has PNAME in its :EXPORT -- that
export INTERNs a fresh symbol there and signals NAME-CONFLICT-IN-EXPORT
against the cold one presented by HOME (EXPORT-INTERNAL's used-by loop,
package.lisp:1473; CLASS-OF: dist homes it in CLOS, but
FUTURE-COMMON-LISP exports it, pkgdcl.lisp:1849, and CLOS
\(:USE FUTURE-COMMON-LISP) -- M3h boot 19).  Direct use only: conflict
checks reach exactly one :USE level, so a distinct symbol two levels
down is legitimate and must NOT be merged.  A HOME that pkgdcl :SHADOWs
keeps its own symbol against (b); (a) still applies (:SHADOW after
:IMPORT-FROM is Genera's spelling of :SHADOWING-IMPORT, pkgdcl.lisp:62
-- the symbol is still the source's)."
  (if (null home)
      home
      (loop with cur = (cold-package-primary home)
            repeat 8
            for next = (or (let ((src (and *cold-package-imports*
                                           (gethash (cons cur pname)
                                                    *cold-package-imports*))))
                             (and src (cold-package-primary src)))
                           (and (not (and *cold-package-shadows*
                                          (gethash (cons cur pname)
                                                   *cold-package-shadows*)))
                                (cold-use-provider-of pname cur)))
            while (and next (not (string= next cur)))
            do (setf cur next)
            finally (return cur))))

(defun cold-resolve-home (pname context-package)
  "Home package PNAME should intern under, seen from CONTEXT-PACKAGE.
Dist homes that PKGDCL never declares (CLTL-INTERNALS: created by a
later system, present in the dist world) are not candidates -- a cold
symbol whose package slot names one would make the FIXUP-SYMBOL-PACKAGE
sweep SIGNAL PACKAGE-NOT-FOUND pre-banner (M3h boot 17); such
references intern under the file's own package instead.  Whatever the
choice, an exporting package the chosen home inherits from claims the
symbol (COLD-ADJUST-HOME-FOR-EXPORTS, M3h boot 19)."
  (let* ((ctx (cold-package-primary context-package))
         (homes (remove-if-not #'cold-declared-package-p
                               (and *cold-symbol-homes*
                                    (gethash pname *cold-symbol-homes*)))))
    (cold-adjust-home-for-exports
     pname
     (cond ((null homes) ctx)
           ((null (rest homes)) (first homes))
           ;; A package's own symbol shadows inherited ones.
           ((member ctx homes :test #'string=) ctx)
           (t (or (let ((strict (and *cold-package-uses*
                                     *cold-package-imports*
                                     (cold-find-visible-home pname ctx
                                                             homes))))
                    ;; Only trust the strict walk when it lands on a
                    ;; dist-verified home -- same contract as the BFS.
                    (and strict (member strict homes :test #'string=)
                         strict))
                  (cold-resolve-through-imports pname ctx homes)
                  (loop for p in *cold-home-priority*
                        when (member p homes :test #'string=) return p)
                  (first homes)))))))

(defun cold-symbol (w pname package-name &key (area "SYMBOL-AREA"))
  "Intern (PNAME, home of PACKAGE-NAME); returns the symbol block VMA.
AREA is SYMBOL-AREA except for the pkgdcl :SAFEGUARDED pre-intern
\(COLD-INTERN-SAFEGUARDED-SYMBOLS), which must land in zone F0."
  (when package-name
    (setf package-name (cold-resolve-home pname package-name)))
  (let ((key (cons pname package-name)))
    (or (and package-name (gethash key (cold-world-symbols w)))
        (let* ((vma (cold-alloc w area 5))
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
               (vma (cold-alloc w area nqs :list)))
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
  "Materialize a decoded array; returns the array HEADER vma.
Multi-dimensional (and overlong 1-D) arrays get the long-prefix format:
[header][long-length][index-offset 0][locative to data][len mult]*dims,
data contiguous after the prefix -- verified against the distribution
world's STANDARD-READTABLE (header 43:0A930002, locative +3 -> +8)."
  (or (gethash varray *cold-object-vmas*)
      (let ((dims (let ((d (varray-dimensions varray)))
                    (if (integerp d) (list d) d))))
        (unless (every #'integerp dims)
          (error "Array dimensions ~S unsupported" dims))
        (multiple-value-bind (type-code element-type packing)
            (cold-array-type w varray)
          (declare (ignore element-type))
          (let* ((len (reduce #'* dims))
                 (per-word (ash 1 packing))
                 (options (varray-options varray))
                 (leader-length
                   (or (varray-option options "LEADER-LENGTH") 0))
                 (leader-list (varray-option options "LEADER-LIST"))
                 (fill-pointer (varray-option options "FILL-POINTER"))
                 (nwords (if (zerop packing) len (ceiling len per-word)))
                 (named-structure
                   (varray-option options "NAMED-STRUCTURE-SYMBOL"))
                 (longp (or (> (length dims) 1) (>= len (ash 1 15))))
                 (prefix-extra (if longp (+ 3 (* 2 (length dims))) 0)))
            (when (and fill-pointer (zerop leader-length))
              (setf leader-length 1))
            (let* ((total (+ (if (zerop leader-length) 0 (1+ leader-length))
                             1 prefix-extra nwords))
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
                                     ;; MAKE-ARRAY :NAMED-STRUCTURE-SYMBOL
                                     ;; puts the symbol in leader 1.
                                     ((and (= i 1) named-structure
                                           (vsym-p named-structure))
                                      named-structure)
                                     (t nil))))
                    (multiple-value-bind (vt vd) (cold-ref w value :area area)
                      (cw-set w (- header 1 i) vt vd)))))
              (cw-set w header (tag 1 (cold-dtp w "HEADER-I"))
                      (logior (ash type-code 26)
                              (if named-structure (ash 1 25) 0)
                              (ash leader-length 15)
                              (if longp
                                  (logior (ash 1 23) (length dims))
                                  len)))
              (when longp
                (let ((fixnum (cold-dtp w "FIXNUM"))
                      (data-base (+ header 1 prefix-extra)))
                  (cw-set w (+ header 1) (tag 0 fixnum) len)
                  (cw-set w (+ header 2) (tag 0 fixnum) 0)  ; index offset
                  (cw-set w (+ header 3)
                          (tag 0 (cold-dtp w "LOCATIVE")) data-base)
                  (loop for d on dims
                        for i from 0
                        do (cw-set w (+ header 4 (* 2 i)) (tag 0 fixnum)
                                   (first d))
                           (cw-set w (+ header 5 (* 2 i)) (tag 0 fixnum)
                                   (reduce #'* (rest d))))))
              ;; Data
              (let ((data-base (+ header 1 prefix-extra)))
                (cond
                  ((varray-words varray)
                   (let ((words (varray-words varray)))
                     (dotimes (i nwords)
                       (let ((low (if (< (* 2 i) (length words))
                                      (aref words (* 2 i)) 0))
                             (high (if (< (1+ (* 2 i)) (length words))
                                       (aref words (1+ (* 2 i))) 0)))
                         (cw-set w (+ data-base i)
                                 (tag 0 (cold-dtp w "FIXNUM"))
                                 (logior low (ash high 16)))))))
                  ((varray-contents varray)
                   (unless (zerop packing)
                     (error "Boxed contents in a packed array"))
                   (let ((contents (varray-contents varray)))
                     (dotimes (i len)
                       (multiple-value-bind (vt vd)
                           (cold-ref w (aref contents i) :area area)
                         (cw-set w (+ data-base i) vt vd)))))
                  (t
                   ;; Uninitialized: NIL for object arrays, 0 for packed.
                   (if (zerop packing)
                       (multiple-value-bind (ntag ndata) (cold-nil-q w)
                         (dotimes (i len)
                           (cw-set w (+ data-base i) ntag ndata)))
                       (dotimes (i nwords)
                         (cw-set w (+ data-base i)
                                 (tag 0 (cold-dtp w "FIXNUM")) 0))))))
              header))))))

;;; ---------------- Function specs and plists ----------------

(defun fspec-key (obj)
  "EQUAL-hashable key for a function spec tree.  Symbol keys resolve
through the home oracle so the same runtime symbol referenced from
different packages yields one key."
  (typecase obj
    (vsym (let ((p (canonical-package-name (vsym-package obj))))
            (if p
                (concatenate 'string (cold-resolve-home (vsym-name obj) p)
                             ":" (vsym-name obj))
                (list :uninterned (vsym-name obj)))))
    (cons (cons (fspec-key (car obj)) (and (cdr obj) (fspec-key (cdr obj)))))
    (t obj)))

(defun cold-follow-cell (w vma)
  "Follow one-q-forward chains from VMA; returns the final cell vma."
  (loop for hops from 0 below 16
        do (multiple-value-bind (tag data) (cw-ref w vma)
             (if (= (tag-type tag) (cold-dtp w "ONE-Q-FORWARD"))
                 (setf vma data)
                 (return vma)))
        finally (return vma)))

(defun cold-read-string (w vma)
  "Read back a cold ART-STRING block."
  (multiple-value-bind (tag data) (cw-ref w vma)
    (declare (ignore tag))
    (let* ((len (ldb (byte 15 0) data))
           (s (make-string len)))
      (dotimes (i len s)
        (multiple-value-bind (wt wd) (cw-ref w (+ vma 1 (floor i 4)))
          (declare (ignore wt))
          (setf (char s i)
                (code-char (ldb (byte 8 (* 8 (mod i 4))) wd))))))))

(defun cold-symbol-pname-at (w sym-vma)
  "Pname string of the symbol block at SYM-VMA, or NIL.  The Q at +0 is
the symbol's DTP-HEADER-P (header type 0) whose data points at the
pname string array."
  (multiple-value-bind (pt pd) (cw-ref w sym-vma)
    (and pt
         (member (tag-type pt) (list (cold-dtp w "HEADER-P")
                                     (cold-dtp w "STRING")
                                     (cold-dtp w "ARRAY")))
         (cold-read-string w pd))))

(defun cold-get-property-q (w sym-vma pname)
  "(values tag data foundp) of the first property on SYM-VMA's plist
whose indicator symbol's pname is PNAME.  Walks the cdr-coded
\(ind val . next) chain the way boot GET does."
  (let ((dtp-list (cold-dtp w "LIST"))
        (dtp-symbol (cold-dtp w "SYMBOL")))
    (multiple-value-bind (pt pd) (cw-ref w (+ sym-vma 3))
      (loop with vma = (and (= (tag-type pt) dtp-list) pd)
            repeat 4096
            while vma
            do (multiple-value-bind (it id) (cw-ref w vma)
                 (multiple-value-bind (vt vd) (cw-ref w (1+ vma))
                   (when (and (= (tag-type it) dtp-symbol)
                              (equal (cold-symbol-pname-at w id) pname))
                     (return (values vt vd t)))
                   (ecase (ldb (byte 2 6) vt)
                     (0 (setf vma (+ vma 2)))
                     (1 (setf vma nil))
                     ((2 3) (multiple-value-bind (nt nd)
                                (cw-ref w (cold-follow-cell w (+ vma 2)))
                              (setf vma (and (= (tag-type nt) dtp-list)
                                             nd)))))))
            finally (return (values nil nil nil))))))

(defun cold-store-contents (w vma tag data)
  "Store TAG:DATA into the Q at VMA preserving the destination's cdr
code, as %P-STORE-CONTENTS does.  Property value cells live inside
cdr-coded plist conses whose value Q carries cdr-normal; a raw cw-set
there zeroes the code, splicing the rest of PROPERTY-LIST-AREA into the
plist, and GET's walk runs off the allocation frontier into unwritten
NULL Qs on the first missing-indicator lookup (M3h boot 23: trap 71 in
GET from DECLARED-STORAGE-CATEGORY on the first :FUNCTION link record)."
  (multiple-value-bind (old-tag old-data) (cw-ref w vma)
    (declare (ignore old-data))
    (cw-set w vma (logior (logand old-tag #xC0) (tag-type tag)) data)))

(defun cold-prepend-property (w sym-vma ind-tag ind-data val-tag val-data)
  "Push an (indicator value) pair onto the plist at SYM-VMA+3; returns the
VMA of the value cell (= PROPERTY-CELL-LOCATION)."
  (let ((block (cold-alloc w "PROPERTY-LIST-AREA" 3 :list)))
    (multiple-value-bind (old-tag old-data) (cw-ref w (+ sym-vma 3))
      (cw-set w block (logior (ash +cdr-next+ 6) (tag-type ind-tag)) ind-data)
      (cw-set w (+ block 1)
              (logior (ash +cdr-normal+ 6) (tag-type val-tag)) val-data)
      (cw-set w (+ block 2) old-tag old-data)
      (cw-set w (+ sym-vma 3) (tag 0 (cold-dtp w "LIST")) block)
      (+ block 1))))

(defun property-fspec-p (fspec)
  (and (consp fspec) (vsym-p (first fspec))
       (string= (vsym-name (first fspec)) "PROPERTY")
       (= (length fspec) 3)
       (vsym-p (second fspec))))

(defun cold-fdefinition-cell (w fspec)
  "The cell a (function FSPEC) locative or fdefine targets.  Symbols use
the function cell; (:PROPERTY sym ind) uses a real property cell; other
list fspecs (flavor methods, internals) get a generator-allocated cell --
their runtime re-fdefinition location differs, which only matters if they
are redefined after boot."
  (let ((key (fspec-key fspec)))
    (or (gethash key (cold-world-fdefs w))
        (setf (gethash key (cold-world-fdefs w))
              (cond
                ((vsym-p fspec) (+ (cold-vsym w fspec) 2))
                ((property-fspec-p fspec)
                 (let ((sym (cold-vsym w (second fspec))))
                   (multiple-value-bind (it id)
                       (cold-ref w (third fspec) :area "PROPERTY-LIST-AREA")
                     (let ((cell (cold-prepend-property
                                  w sym it id
                                  (tag 0 (cold-dtp w "NULL")) 0)))
                       ;; Unbound convention: dtp-null pointing at the cell.
                       (cold-store-contents w cell (tag 0 (cold-dtp w "NULL"))
                                            cell)
                       cell))))
                ((consp fspec)
                 ;; (fspec-list . cell) block; the locative points at the cell.
                 (multiple-value-bind (ft fd) (cold-ref w fspec)
                   (let ((block (cold-alloc w "PERMANENT-STORAGE-AREA" 2
                                            :list)))
                     (cw-set w block
                             (logior (ash +cdr-normal+ 6) (tag-type ft)) fd)
                     (cw-set w (1+ block) (tag 0 (cold-dtp w "NULL"))
                             (1+ block))
                     (1+ block))))
                (t (error "Unhandled function spec ~S" fspec)))))))

;;; ---------------- Instances (cold marker lists) ----------------

(defun cold-instance-marker (w)
  "The unique MAKE-INSTANCE-COLD marker: an uninterned symbol, also stored
as SI:*COLD-MAKE-INSTANCE-MARKER*'s value so first-boot code recognizes and
rebuilds the placeholder lists (cold-load.lisp:404, debugger/handlers.lisp
BOOTSTRAP-FASD-INSTANCES)."
  (let ((marker (cold-world-instance-marker w)))
    (if (plusp marker)
        marker
        (let ((sym (cold-symbol w "COLD-INSTANCE-MARKER" nil))
              (var (cold-symbol w "*COLD-MAKE-INSTANCE-MARKER*"
                                "SYSTEM-INTERNALS")))
          (cw-set w (1+ var) (tag 0 (cold-dtp w "SYMBOL")) sym)
          (setf (cold-world-instance-marker w) sym)))))

(defun cold-instance (w vinstance area)
  "(marker flavor . init-plist) placeholder list; returns its VMA."
  (or (gethash vinstance *cold-object-vmas*)
      (let* ((plist (vinstance-plist vinstance))
             (n (+ 2 (length plist)))
             (vma (cold-alloc w area n :list))
             (dtp-symbol (cold-dtp w "SYMBOL")))
        (setf (gethash vinstance *cold-object-vmas*) vma)
        (cw-set w vma (tag +cdr-next+ dtp-symbol) (cold-instance-marker w))
        (multiple-value-bind (ft fd)
            (cold-ref w (vinstance-flavor vinstance) :area area)
          (cw-set w (1+ vma)
                  (logior (ash (if plist +cdr-next+ +cdr-nil+) 6)
                          (tag-type ft))
                  fd))
        (loop for p on plist
              for i from 2
              do (multiple-value-bind (pt pd) (cold-ref w (car p) :area area)
                   (cw-set w (+ vma i)
                           (logior (ash (if (cdr p) +cdr-next+ +cdr-nil+) 6)
                                   (tag-type pt))
                           pd)))
        vma)))

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
    (veval
     (funcall *cold-load-time-eval* w (veval-form obj)))
    (vsym (cold-symbol-ref w obj))
    (vloc
     (let ((target (vloc-target obj)))
       (values (tag 0 (cold-dtp w "LOCATIVE"))
               (ecase (vloc-kind obj)
                 (:value (unless (vsym-p target)
                           (error "Value locative to ~S" target))
                         (1+ (cold-vsym w target)))
                 (:function (cold-fdefinition-cell w target))))))
    (vchar
     ;; The only style the cold set carries is the plain default
     ;; (NIL.NIL.NIL, no attributes) = style index 0; anything else would
     ;; need the warm style-interning machinery.
     (let ((style (vchar-style obj)))
       (unless (or (null style)
                   (and (vfalse-p (vstyle-family style))
                        (vfalse-p (vstyle-face style))
                        (vfalse-p (vstyle-size style))
                        (vfalse-p (vstyle-attributes style))))
         (error "Styled character ~S not yet supported" obj)))
     ;; Char word: bits(4@28) style(12@16) charset(8@8) subindex(8@0)
     ;; (%%CHAR-* byte specs).  Op #o53 chars carry the full 16-bit code
     ;; in VCHAR-CODE with no charset; op #o54 chars carry the subindex
     ;; plus a charset whose slot the generator must bake in.
     (values (tag 0 (cold-dtp w "CHARACTER"))
             (logior (ash (or (vchar-bits obj) 0) 28)
                     (if (vchar-charset obj)
                         (ash (cold-charset-slot obj) 8)
                         0)
                     (vchar-code obj))))
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
    (vfun
     (values (tag 0 (cold-dtp w "COMPILED-FUNCTION")) (cold-fun w obj)))
    (vembed
     (unless *cold-cca-base*
       (error "Embedded constant ~S outside a compiled function" obj))
     (values (tag 0 (cold-dtp w (aref *cold-embed-types*
                                      (vembed-dtp-code obj))))
             (+ *cold-cca-base* (vembed-offset obj))))
    (vnative
     (values (tag 0 (cold-dtp w "SPARE-IMMEDIATE-1")) (vnative-word obj)))
    (vraw (values (vraw-tag obj) (vraw-data obj)))
    (vinstance
     (values (tag 0 (cold-dtp w "LIST")) (cold-instance w obj area)))
    (character  ; strings decode to CL chars only via veval forms
     (values (tag 0 (cold-dtp w "CHARACTER")) (char-code obj)))))
