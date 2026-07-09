;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: the driver.
;;;
;;; The cold set and its order.  The 85 plain files mirror
;;; worldtool/genera/m2-compile.lisp *cold-set-plain-files*, which follows
;;; the SCT declaration order of the system-internals subsystem
;;; (sys/sys/sysdcl.lisp:119-360).  The four :readtable-type modules load
;;; between RTC and LDATA, matching sysdcl's module order.  SYS:SYS;PKGDCL
;;; is :lisp-read-only -- the generator reads it as source (M3f).
;;;
;;; Three files the SI-subsystem crossing missed, compiled via
;;; m2-compile.lisp *cold-set-late-found-files*: SYS:SYS;LISP-DATABASE-COLD
;;; (PROCLAIM and the DEFVAR-1 boot bookkeeping; after "SYS: SYS; EVAL"),
;;; SYS:DEBUGGER;ITRAP-DISPATCH (the trap handlers, incl. the entry-T
;;; catch-all that fills the trap page; after "SYS: SYS; WIRED"), and
;;; SYS:GC;IGC-COLD (M3e finding: %GC-FLIP-READY / GC-RECLAIMED-OLDSPACE
;;; that every inline allocation site tests, plus %HARDWARE-TRANSPORT-TRAP
;;; for trap vector 2630; after ITRAP-DISPATCH so DEF-TRAP-HANDLER exists).

(in-package #:worldtool)

(defparameter *cold-load-order*
  '("SYS: IO; RDDEFS" "SYS: SYS; WIRED-EVENT-DEFS" "SYS: SYS; IARITHDEFS"
    "SYS: I-SYS; SYSDEF" "SYS: I-SYS; SYSDF1" "SYS: SYS2; BARS"
    "SYS: STORAGE; DISK-DEFINITIONS" "SYS: SYS2; BIGDEFS" "SYS: SYS2; LNUMER-DEFS"
    "SYS: METERING; METERING-COLD" "SYS: METERING; METERING-MACROS"
    "SYS: I-SYS; BLOCK-FUNCTIONS" "SYS: SYS; AARRAY" "SYS: SYS2; ADVISE"
    "SYS: SYS; COLD-LOAD" "SYS: SYS; COMMAND-LOOP" "SYS: SYS; EXPAND-DO"
    "SYS: IO; DRIBBL" "SYS: SYS2; ENCAPS" "SYS: SYS; EVAL"
    "SYS: SYS; LISP-DATABASE-COLD" "SYS: SYS; FSPEC"
    "SYS: SYS2; HASH" "SYS: SYS2; HEAP" "SYS: IO; INTERACTIVE-STREAM"
    "SYS: IO; INPUT-EDITOR" "SYS: IO; ITERATORS" "SYS: SYS2; LET"
    "SYS: SYS; LISPFN" "SYS: SYS; LTOP" "SYS: SYS2; MACLSP"
    "SYS: SYS; MACROEXPAND" "SYS: SYS2; MEMORY-COLD"
    "SYS: EMBEDDING; RPC; OCTET-STRUCTURE-RUNTIME" "SYS: SYS; PACKAGE"
    "SYS: SYS2; PLANE" "SYS: IO; PRINT" "SYS: IO; QIO" "SYS: IO; READ"
    "SYS: IO; READERS" "SYS: SYS2; RESOUR" "SYS: SYS2; SELEV" "SYS: SYS; SORT"
    "SYS: SYS; STANDARD-VALUES" "SYS: SYS2; STORAGE-CATEGORIES"
    "SYS: SYS2; STRING" "SYS: SYS2; STRUCT-COLD"
    "SYS: IO; UNIX-TRANSLATING-STREAMS" "SYS: SYS; WIRED-EVENT-LOG"
    "SYS: IO; RTC"
    ;; :readtable-type modules (compiled by SI:RTC-FILE)
    "SYS: IO; RDTBL" "SYS: CLCP; READTABLE" "SYS: CLCP; ANSI-READTABLE"
    "SYS: EMBEDDING; RPC; C-READTABLE"
    "SYS: SYS; LDATA" "SYS: SYS; LCODE" "SYS: SYS; I-ALLOCATE"
    "SYS: SYS; ALLOCATE-COMMON" "SYS: SYS; ICONS" "SYS: SYS; OBJECTS"
    "SYS: SYS; DESCRIBE" "SYS: SYS; COLD-LOAD-STREAM" "SYS: SYS; IFEPIO"
    "SYS: SYS; IPRIM" "SYS: SYS; ISTACK" "SYS: SYS; LARITH" "SYS: SYS2; DOUBLE"
    "SYS: SYS2; COMPLEX" "SYS: SYS; WIRED" "SYS: DEBUGGER; ITRAP-DISPATCH"
    ;; The compiled error tables (COMPILE-ERROR-TABLES output; original
    ;; *TRAP-DISPATCH-TABLE-FILE*): SETQs of DBG:*TRAP-DISPATCH-TABLES* /
    ;; *TRAP-ON-EXIT-MICROSTATES* / *TRAP-DISPATCH-TABLE-VERSIONS*, which
    ;; SYSTEM-STARTUP's INITIALIZE-ERROR-TRAP-DISPATCH AREFs pre-banner
    ;; (M3h boot-7 trap).  The REV~D-ERROR-TABLE.LISP sources and the
    ;; original ibin are lost; this one was re-dumped 2026-07-04 from the
    ;; user's running Genera 8.5 with the same DUMP-FORMS-TO-FILE call.
    ("SYS: I-SYS; TRAP-DISPATCH-TABLE" . "ibin")
    "SYS: GC; IGC-COLD"
    "SYS: I-SYS; WIRED-CONSOLE"
    "SYS: I-SYS; WIRED-SCREEN" "SYS: STORAGE; STORAGE" "SYS: STORAGE; USER-STORAGE"
    "SYS: STORAGE; STACK-WIRING" "SYS: STORAGE; DISK-DRIVER"
    "SYS: STORAGE; USER-DISK-DRIVER" "SYS: STORAGE; EMBEDDED-DISK-DRIVER"
    "SYS: STORAGE; VLM-DISK-UTILITIES" "SYS: IO; LMINI" "SYS: IO; USEFUL-STREAMS"
    "SYS: I-SYS; INTERRUPTS" "SYS: I-SYS; V-INTERRUPTS" "SYS: I-SYS; AUDIO"
    "SYS: EMBEDDING; EMB-BUFFER" "SYS: EMBEDDING; EMB-QUEUE"
    "SYS: EMBEDDING; EMB-MESSAGE-CHANNEL"
    ;; In the original cold set (dist has its functions wired) but the
    ;; .vbin was lost; recompiled from source in the user's Genera 8.5
    ;; 2026-07-04.  initialize-disk's VLM branch calls
    ;; INITIALIZE-EMBEDDED-NETWORK pre-banner (M3h boot-6 trap).
    "SYS: NETWORK; EMB-ETHERNET-DRIVER"
    ;; The CLCP crossing the SI-subsystem derivation missed entirely (M3h
    ;; boot-10 trap: BUILD-INITIAL-PACKAGES -> COPYLIST -> unbound
    ;; CLI:LAST-1).  Membership oracle: sys/mini-alists.lisp -- QLD's
    ;; INNER-SYSTEM-FILE-ALIST loads seqfns/arrayfns/stringfns/numerics/
    ;; error ON TOP of the cold load (their .vbins are the only CLCP
    ;; binaries in the distribution), so the CLCP files it does NOT list
    ;; that cold code calls pre-banner were in the cold load itself:
    ;;   PERMANENT-LINKS -- "simulated by the cold load generator"
    ;;     (its header comment); records SI:*LINKED-SYMBOL-CELLS* triples
    ;;     that BOOTSTRAP-FORWARD-SYMBOL-CELLS forwards at first boot
    ;;     (CL:*TERMINAL-IO* <-> ZL:TERMINAL-IO, *PACKAGE* <-> PACKAGE...)
    ;;   FUNCTIONS -- CL:EQUAL/MAPC/GENSYM/CLI:PUTPROP/SET-GETF/...
    ;;   LISTFNS -- CLI:LAST-1 (COPYLIST's (cdr (last list)) transform),
    ;;     MEMBER-EQUAL, ASSOC-EQUAL, ...
    ;;   IOFNS -- LISP:MAKE-SYNONYM-STREAM (the banner's SYN-TERMINAL-IO),
    ;;     CLI:FOLLOW-SYNONYM-STREAM, WRITE-CHAR/WRITE-STRING/FRESH-LINE;
    ;;     its own comments record cold-load history (Hornig & Dodds
    ;;     10/09/92).  Compiled from source in the user's Genera 8.5.
    "SYS: CLCP; PERMANENT-LINKS" "SYS: CLCP; FUNCTIONS"
    "SYS: CLCP; LISTFNS" "SYS: CLCP; IOFNS"))

;;; ---- M3f: finalization and the full pipeline -----------------------------

(defun cold-wired-ranges (w)
  "VMA ranges emitted as WIRED map entries: the architectural wired region
(trap page, comm blocks, NIL/T, region tables, wired control tables, the
initial stack group), the SAFEGUARDED region (ITRAP-DISPATCH's handlers
materialize there -- the page-fault handler cannot itself be pageable;
ground truth wires its F0000000 map entries), and the initial
control/binding stacks.  Everything else -- heap, page-table space --
pages on demand."
  (loop for r across (cold-world-regions w)
        for area = (strip-package
                    (cold-area-name (cold-area w (cold-region-area r))))
        when (member area '("WIRED-CONTROL-TABLES" "SAFEGUARDED-OBJECTS-AREA"
                            "CONTROL-STACK-AREA" "BINDING-STACK-AREA")
                     :test #'string=)
          collect (cons (cold-region-origin r)
                        (+ (cold-region-origin r) (cold-region-length r)))))

(defun cold-deferred-defvar-parts (form)
  "Match the BOUNDP-guarded deferred defvar shape COLD-DO-DEFVAR emits,
\(IF (BOUNDP 'sym) NIL (SET 'sym valform)); returns (values sym valform)
or NIL.  Plain SETs (defconst/setq deferrals) are deliberately not
matched: stamping those would double-evaluate side-effecting forms at
boot, whereas the guarded shape no-ops once the symbol is bound."
  (flet ((head-p (x name) (and (consp x) (vsym-p (first x))
                               (string= (vsym-name (first x)) name))))
    (when (and (head-p form "IF")
               (= (length form) 4)
               (head-p (second form) "BOUNDP")
               (head-p (fourth form) "SET"))
      (let ((guard-q (second (second form)))
            (set-form (fourth form)))
        (when (and (head-p guard-q "QUOTE")
                   (head-p (second set-form) "QUOTE")
                   (vsym-p (second (second set-form))))
          (values (second (second set-form)) (third set-form)))))))

(defun cold-reconcile-linked-defvars (w)
  "M3h boot 21: BOOTSTRAP-LINK-SYMBOL-CELLS FERRORs \"Can't link two
cells with different values.\" when a permanent-links record's cells are
both bound with non-EQ contents (sys2/memory-cold.lisp:421-426).  LDATA's
\(DEFVAR *READTABLE* *COMMON-LISP-READTABLE*) loads before permanent-links
records the *READTABLE* - READTABLE value link (rdtbl's SETQ made
READTABLE the ZL readtable), so the eager defvar stamp shipped exactly
that fatal state.  The genuine first boot never evaluates the init: the
link pass copies the bound side into the unbound one first, and the
deferred DEFVAR-1 no-ops on the now-bound cell.  Revert the
defvar-stamped side to unbound and re-defer its guarded SET; a conflict
this can't attribute to exactly one defvar stamp is a generator bug.
Returns the number reverted."
  (let ((fixed 0)
        (stamps (cold-world-defvar-stamps w)))
    (dolist (rec (reverse (cold-world-linked-cells w)) fixed)
      (destructuring-bind (from to type) rec
        (when (string= (vsym-name type) "VARIABLE")
          (multiple-value-bind (ft fd fb) (cold-symbol-value-q w from)
            (multiple-value-bind (tt td tb) (cold-symbol-value-q w to)
              (when (and fb tb (not (cold-q-eq ft fd tt td)))
                (let ((fs (gethash (cold-vsym w from) stamps))
                      (ts (gethash (cold-vsym w to) stamps)))
                  (multiple-value-bind (victim set-form)
                      (cond ((and fs ts)
                             (error "Value link ~A - ~A: both sides are ~
defvar-stamped with different values"
                                    (vsym-name from) (vsym-name to)))
                            (fs (values from fs))
                            (ts (values to ts))
                            (t (error "Value link ~A - ~A: both cells ~
bound with different values and neither is a defvar stamp"
                                      (vsym-name from) (vsym-name to))))
                    (cw-set w (cold-value-cell w victim)
                            (tag 0 (cold-dtp w "NULL"))
                            (cold-vsym w victim))
                    ;; A retry-pass stamp's BOUNDP-guarded original is
                    ;; still on the deferred list; only eager
                    ;; COLD-DO-DEFVAR stamps need their SET re-deferred.
                    (unless (eq set-form :already-deferred)
                      (let ((*cold-default-package* "SYSTEM-INTERNALS"))
                        (cold-defer w (list (si-vsym "IF")
                                            (list (si-vsym "BOUNDP")
                                                  (list (si-vsym "QUOTE")
                                                        victim))
                                            nil
                                            set-form)
                                    "linked defvar re-deferred")))
                    (incf fixed)))))))))))

(defun cold-retry-deferred-defvars (w)
  "M3h boot 18: a cold file's DEFVAR initializer that was unevaluable at
vbin-load time (AREA-FOR-NEW-SYMBOLS's is SYMBOL-AREA, an area variable
the machinery pass stamps AFTER the load) deferred a BOUNDP-guarded SET
-- but MAKE-SYMBOL reads the variable inside BUILD-INITIAL-PACKAGES,
before the deferred list is MAPCed.  Retry each such form now, post
machinery: whatever evaluates is stamped, and the still-guarded deferred
form no-ops at boot.  Value-linked symbols are stamped like any other --
LISP-INITIALIZE-FIRST-TIME's init loop SETs every still-unbound listed
variable to its RAW init form before the link pass
\(sys/cold-load.lisp:527-528), so leaving DEFAULT-CONS-AREA unbound
shipped it the symbol WORKING-STORAGE-AREA against *DEFAULT-CONS-AREA*'s
8, the exact both-bound-different FERROR the boot-21 blanket skip meant
to prevent (M3h boot 22).  Each stamp records provenance so the
reconcile pass running next can revert it if it does conflict.
Returns the number stamped."
  (let ((stamped 0))
    (dolist (entry (reverse (cold-world-deferred w)))
      (destructuring-bind (pkg . form) entry
        (multiple-value-bind (sym valform) (cold-deferred-defvar-parts form)
          (when (and sym
                     (not (nth-value 2 (cold-symbol-value-q w sym))))
            (let ((*cold-default-package* pkg))
              (multiple-value-bind (tag data) (cold-eval-value w valform)
                (when tag
                  (cold-set-symbol-value w sym tag data)
                  (setf (gethash (cold-vsym w sym)
                                 (cold-world-defvar-stamps w))
                        :already-deferred)
                  (incf stamped))))))))
    ;; SI:COLD-LOAD-FUNCTION-PROPERTY-LISTS has no initializer anywhere
    ;; (fspec.lisp:385), but FUNCTION-SPEC-DEFAULT-HANDLER's cold path
    ;; reads and PUSHes it whenever *FUNCTION-SPEC-HASH-TABLES* is NIL
    ;; (fspec.lisp:403,413); it is on the obsolete-after-QLD registry
    ;; (cold-load.lisp:268), so the genuine cold world shipped it bound.
    (let ((clfpl (si-vsym "COLD-LOAD-FUNCTION-PROPERTY-LISTS")))
      (unless (nth-value 2 (cold-symbol-value-q w clfpl))
        (multiple-value-bind (nt nd) (cold-nil-q w)
          (cold-set-symbol-value w clfpl nt nd))
        (incf stamped)))
    stamped))

(defun cold-finalize (w)
  "Everything between the last vbin and emit (M3f):
1. the PKGDCL pass stores SI:BUILD-INITIAL-PACKAGES (cold-pkg.lisp),
   withholding :RELATIVE-NAMES triples (M3h boot 14) -- it runs first
   so the deferred list can carry their re-establishment forms;
2. the first-boot Q patches become (SYS:%P-STORE-CONTENTS locative form)
   forms ahead of the deferred forms proper (%P-STORE-CONTENTS is a
   DEFUPRIM, iprim.lisp:209, so EVAL can apply it at boot; locatives
   self-evaluate);
3. the captured deferred list materializes as the value of
   SI:*COLD-LOAD-DEFERRED-FORMS* -- LISP-INITIALIZE-FIRST-TIME MAPCs EVAL
   over it after BUILD-INITIAL-PACKAGES (cold-load.lisp:543-547); the
   withheld relative names go last as (SI:PKG-ADD-RELATIVE-NAME pkg
   name target) calls, the cold-safe path (MAKE-PACKAGE's own
   :RELATIVE-NAMES handling COPYTREEs a dotted (name . package) alist
   and LENGTH dies in the pre-banner ENDP trap);
4. SI:*VALUE-CELLS-TO-LOCALIZE-FIRST* / SI:*LINKED-SYMBOL-CELLS* := NIL
   (the boot localize pass reads both, memory-cold.lisp:286-297; ground
   truth has them NIL);
5. every KEYWORD-package symbol is forwarded self-evaluating (cold-eval),
   so BUILD-INITIAL-PACKAGES can EVAL those forms at first boot.  Done
   last: the PKGDCL pass and the deferred-list materialization both
   intern keywords.
Returns (values deferred-count patch-count package-count)."
  (with-cold-materializer (w)
    (let* ((*cold-load-time-eval* #'cold-operand-eval)
           (package-count
             (cold-load-pkgdcl w (sys-pathname "SYS: SYS; PKGDCL" "lisp")))
           (revived (cold-retry-deferred-defvars w))
           (reconciled (cold-reconcile-linked-defvars w))
           (patches (reverse (cold-world-patches w)))
           (deferred (reverse (cold-world-deferred w)))
           (store (make-vsym "SYSTEM" "%P-STORE-CONTENTS"))
           (loc-tag (tag 0 (cold-dtp w "LOCATIVE")))
           (add-name (make-vsym "SYSTEM-INTERNALS" "PKG-ADD-RELATIVE-NAME"))
           (entries
             (append
              (loop for (vma pkg form) in patches
                    collect (cons pkg (list store (make-vraw loc-tag vma)
                                            form)))
              deferred
              (loop for triple in (reverse (cold-world-relative-names w))
                    collect (cons "SYSTEM-INTERNALS"
                                  (cons add-name triple))))))
      (multiple-value-bind (spine-tag spine-data) (cold-nil-q w)
        (dolist (entry (reverse entries))
          (let ((*cold-default-package* (car entry)))
            (multiple-value-bind (ft fd)
                (cold-ref w (cdr entry) :area "WORKING-STORAGE-AREA")
              (setf spine-data (cold-cons w ft fd spine-tag spine-data)
                    spine-tag (tag 0 (cold-dtp w "LIST"))))))
        (cold-set-symbol-value
         w (make-vsym "SYSTEM-INTERNALS" "*COLD-LOAD-DEFERRED-FORMS*")
         spine-tag spine-data))
      ;; Anything deferred or patched WHILE materializing the list would
      ;; never execute -- insist the capture was complete.
      (unless (and (= (length (cold-world-patches w)) (length patches))
                   (= (length (cold-world-deferred w)) (length deferred)))
        (error "~D patch~:P / ~D deferral~:P noted while materializing ~
the deferred list"
               (- (length (cold-world-patches w)) (length patches))
               (- (length (cold-world-deferred w)) (length deferred))))
      (multiple-value-bind (nt nd) (cold-nil-q w)
        (cold-set-symbol-value
         w (make-vsym "SYSTEM-INTERNALS" "*VALUE-CELLS-TO-LOCALIZE-FIRST*")
         nt nd))
      ;; The permanent-links load pass recorded (from to :variable/:function)
      ;; triples; BOOTSTRAP-FORWARD-SYMBOL-CELLS walks this list at first
      ;; boot and performs the actual cell forwarding
      ;; (sys2/memory-cold.lisp:286-292).  The dist's NIL is the CONSUMED
      ;; state -- a fresh world must carry the records.
      (multiple-value-bind (lt ld)
          (cold-ref w (reverse (cold-world-linked-cells w))
                    :area "WORKING-STORAGE-AREA")
        (cold-set-symbol-value
         w (make-vsym "SYSTEM-INTERNALS" "*LINKED-SYMBOL-CELLS*") lt ld))
      ;; 5. Every keyword must self-evaluate before BUILD-INITIAL-PACKAGES
      ;;    EVALs the DEFPACKAGE-INTERNAL forms the PKGDCL pass stored
      ;;    (package.lisp:2393, well before BOOTSTRAP-FORWARD-SYMBOL-CELLS).
      ;;    Done last: the PKGDCL pass and the deferred-list
      ;;    materialization above both intern keywords.
      (cold-forward-all-keywords w)
      (when (plusp revived)
        (format t "~&  ~D deferred defvar value~:P stamped at finalize~%"
                revived))
      (when (plusp reconciled)
        (format t "~&  ~D linked defvar stamp~:P reverted to unbound~%"
                reconciled))
      (values (length entries) (length patches) package-count))))

(defun cold-build-world (w &key reference)
  "The full generator pipeline after MAKE-SKELETON-WORLD: heap regions,
the 88-file cold load, the wired machinery (REFERENCE world supplies the
IFEP vector grafts), finalization.  One materializer scope end to end so
deferred forms alias the structure the load materialized.  Returns the
values of COLD-FINALIZE."
  (cold-add-heap-regions w)
  (with-cold-materializer (w)
    (let ((failures (cold-load-cold-set w)))
      (unless (zerop failures)
        (error "~D fixup~:P never resolved" failures)))
    (cold-build-wired-machinery w :reference reference)
    (cold-finalize w)))
