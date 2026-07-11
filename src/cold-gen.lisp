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
    ;; Post-M3h: "parts of the compiler needed early on in system
    ;; building" -- band roster 16/17 defuns 882-cold in the dist (the
    ;; 17th, DISASSEMBLE-DECODE-LOCATIVE, is 882 by hand: fcell
    ;; 05->88213FBF).  PRINT-LOCATIVE (io/print.lisp:2029) calls
    ;; DISASSEMBLE-DECODE-LOCATIVE for every locative it prints, so
    ;; every post-banner error report ("... referencing ~S") recursed
    ;; on its unbound fcell to an AUX-HALT.  CONSTANT-FORM-P is "called
    ;; by the debugger & flavors, so must be loaded early" -- before
    ;; the FLAVOR block.  Runtime deps verified: the
    ;; *SYSTEM-SYMBOL-CELL-TABLE* pair is SETQ'd by cold
    ;; BOOTSTRAP-FORWARD-SYMBOL-CELLS, LT:NAMED-CONSTANT-P is the boot
    ;; FSET stub NAMED-CONSTANT-P-COLD (cold-load.lisp:220).  No vbin
    ;; ships; the user compiles it (m2-compile
    ;; *cold-set-late-found-files*).
    "SYS: COMPILER; INNER"
    ;; Inner flavor runtime (M3h boot 26).  Pass 1 of
    ;; BOOTSTRAP-FORWARD-SYMBOL-CELLS FDEFINEDPs every walked CCA name;
    ;; method-family names validate through (GET head
    ;; 'FUNCTION-SPEC-HANDLER) -> METHOD-FUNCTION-SPEC-HANDLER, whose
    ;; failure arm CHECK-ARG-1 is warm-only -- so the handler machinery
    ;; itself was cold.  Proof chain: dribbl is in no QLD mini-alist yet
    ;; (FDEFINEDP '(FLAVOR:METHOD :TYO SI:DRIBBLE-STREAM)) is T in the
    ;; user's warm Genera; flavor/bootstrap.lisp converts
    ;; *UNDEFINED-METHOD-HASH-TABLE* and
    ;; *STANDARDIZED-GENERIC-FUNCTION-NAMES* FROM LISTS (the pre-banner
    ;; representations) and verifies pre-compiler "combined method
    ;; bootstrap guesses" -- composition ran cold too.  QLD's
    ;; INNER-SYSTEM-FILE-ALIST reloads these on top, like the CLCP
    ;; pattern.  flavor/make stays warm (FSET stub MAKE-INSTANCE ->
    ;; MAKE-INSTANCE-COLD is the bridge); bootstrap/other/error/update
    ;; are QLD-side.  Deferred DEFFLAVOR-INTERNAL of VANILLA-FLAVOR must
    ;; precede the first deferred composition, hence the whole block
    ;; loads before DRIBBL.
    "SYS: FLAVOR; GLOBAL" "SYS: FLAVOR; DEFFLAVOR" "SYS: FLAVOR; DEFGENERIC"
    "SYS: FLAVOR; DEFMETHOD" "SYS: FLAVOR; COMPOSE" "SYS: FLAVOR; COMBINE"
    "SYS: FLAVOR; HANDLE" "SYS: FLAVOR; CTYPES" "SYS: FLAVOR; VANILLA"
    "SYS: IO; DRIBBL" "SYS: SYS2; ENCAPS" "SYS: SYS; EVAL"
    "SYS: SYS; LISP-DATABASE-COLD" "SYS: SYS; FSPEC"
    ;; Boot 38: five files removed from the cold set below (HASH, HEAP,
    ;; INTERACTIVE-STREAM, STANDARD-VALUES, VLM-DISK-UTILITIES).  A full
    ;; band-roster reconciliation of the cold set against the dist
    ;; (coldset-audit.lisp -- the oracle) proved none of them was ever
    ;; genuinely cold, and each carries a deferred
    ;; COMPILE-FLAVOR-METHODS-LOAD-TIME whose component flavor is DEFFLAVORed
    ;; ONLY on the QLD side -- so COMPOSE-FLAVOR-COMBINATION's missing-component
    ;; WARN fires pre-banner, where any WARN is fatal (streams unbound until
    ;; the banner, by design).  Boot 38 died on hash's (EQ-HASH-TABLE ...)
    ;; CFM missing FCL:HASH-TABLE (QLD-only sys2/tables.lisp); the queued
    ;; siblings are heap->ERROR, interactive-stream->character-sets,
    ;; standard-values->ERROR, vlm-disk-utilities->DISK-ERROR.  See the new
    ;; check-deferred-flavor-composition gate (cold-diff.lisp) -- the
    ;; systematic detector for this class.
    ;;
    ;; Boot 39: a sixth file removed -- io/INPUT-EDITOR, interactive-
    ;; stream's cluster sibling.  Band oracle: its functions are uniformly
    ;; 0x8223 QLD-band in the dist (WITH-IE-TYPEOUT-INTERNAL 05:822321AE,
    ;; IE-MAKE-BLIP 05:82232974) -- never genuinely cold, missed in boot
    ;; 38's batch.  Its DEFUN-IE macro forms defer FDEFINEs of
    ;; (DEFUN-IN-FLAVOR <name> INTERACTIVE-STREAM ...) specs; at boot the
    ;; FDEFINE arm of METHOD-FUNCTION-SPEC-HANDLER (defmethod.lisp:943-948)
    ;; does (OR (FIND-FLAVOR 'INTERACTIVE-STREAM NIL) (ERROR "~S is not the
    ;; name of a flavor...")) -- and INTERACTIVE-STREAM was DEFFLAVORed only
    ;; by the file boot 38 pruned, so the ERROR fires fatally pre-banner.
    ;; input-editor's PRINTING-INPUT-EDITOR DEFFLAVOR-INTERNAL + CFM
    ;; (input-editor.lisp:2315, composes on INTERACTIVE-STREAM) was the next
    ;; queued landmine of the boot-38 class; the prune removes both.  The
    ;; check-deferred-flavor-composition gate now also detects the method-
    ;; family-FDEFINE-on-undefined-flavor variant (the class that let IE-
    ;; CHARACTER slip through boot 38's CFM-only detector).
    "SYS: IO; ITERATORS" "SYS: SYS2; LET"
    "SYS: SYS; LISPFN" "SYS: SYS; LTOP" "SYS: SYS2; MACLSP"
    "SYS: SYS; MACROEXPAND" "SYS: SYS2; MEMORY-COLD"
    "SYS: EMBEDDING; RPC; OCTET-STRUCTURE-RUNTIME" "SYS: SYS; PACKAGE"
    "SYS: SYS2; PLANE" "SYS: IO; PRINT" "SYS: IO; QIO" "SYS: IO; READ"
    "SYS: IO; READERS" "SYS: SYS2; RESOUR" "SYS: SYS2; SELEV" "SYS: SYS; SORT"
    "SYS: SYS2; STORAGE-CATEGORIES"
    ;; io/stream IS genuinely cold and was missing (band-audit-proven, and
    ;; confirmed necessary by check-deferred-flavor-composition: without it
    ;; the gate FAILs on four deferred CFMs).  The already-cold io/unix-
    ;; translating-streams.lisp (below) DEFFLAVORs the TCP embedded-network
    ;; unix-character-stream flavors (8BIT-BINARY-STREAM-ASSOCIATED-UNIX-
    ;; CHARACTER-{,INPUT,OUTPUT}-STREAM) on components BIDIRECTIONAL-STREAM /
    ;; UNBUFFERED-LINE-INPUT-MIXIN / LINE-OUTPUT-STREAM-MIXIN -- all
    ;; DEFFLAVORed only in io/stream.lisp (245/359/429).  Their deferred
    ;; COMPILE-FLAVOR-METHODS-LOAD-TIME would hit COMPOSE-FLAVOR-COMBINATION's
    ;; missing-component WARN (fatal pre-banner) without stream's flavors.
    ;; It occupies its sysdcl main-group slot here (right after storage-
    ;; categories, before string) -- a hard constraint: stream's DEFFLAVOR-
    ;; INTERNALs must precede unix-translating-streams' CFMs in deferred
    ;; order.  (The SI:STREAM/SYNONYM-STREAM banner path via MAKE-SYN-STREAM,
    ;; cold-load.lisp:553, also lives here, but iofns.lisp:2116's SYNONYM-
    ;; STREAM DEFFLAVOR is inside a #||...||# block comment and never
    ;; compiles -- the real driver is the unix-character-stream cluster.)
    "SYS: IO; STREAM"
    "SYS: SYS2; STRING" "SYS: SYS2; STRUCT-COLD"
    "SYS: IO; UNIX-TRANSLATING-STREAMS" "SYS: SYS; WIRED-EVENT-LOG"
    "SYS: IO; RTC"
    ;; :readtable-type modules (compiled by SI:RTC-FILE)
    "SYS: IO; RDTBL" "SYS: CLCP; READTABLE" "SYS: CLCP; ANSI-READTABLE"
    "SYS: EMBEDDING; RPC; C-READTABLE"
    "SYS: SYS; LDATA"
    ;; sysdcl.lisp:320 puts ldefsel right after ldata in l-main.  Its
    ;; runtime helpers are first-boot obligations: plain-DEFSELECT
    ;; expansions in cold files leave (DEFSELECT-CONS-WHICH-OPERATIONS
    ;; 'methods 'tail) eval-at-load-time operands (ldefsel.lisp:143)
    ;; that materialize as first-boot patches, and the :which-operations
    ;; dispatch calls DEFSELECT-INVOKE-WHICH-OPERATIONS.  Cold-band
    ;; proof: dist fcells of both forward into the same 0x882xxxxx band
    ;; as FDEFINEDP (cold), not SUBSTITUTE-IF's 0x823xxxxx (QLD) --
    ;; INNER-SYSTEM-FILE-ALIST membership ("Needed by everything in
    ;; sight") does not exclude coldness, the boot-26 lesson (M3h boot
    ;; 28).
    "SYS: SYS2; LDEFSEL"
    "SYS: SYS; LCODE" "SYS: SYS; I-ALLOCATE"
    "SYS: SYS; ALLOCATE-COMMON" "SYS: SYS; ICONS" "SYS: SYS; OBJECTS"
    "SYS: SYS; DESCRIBE" "SYS: SYS; COLD-LOAD-STREAM" "SYS: SYS; IFEPIO"
    "SYS: SYS; IPRIM" "SYS: SYS; ISTACK" "SYS: SYS; LARITH"
    ;; Post-M3h: Ivory software-float support (FP exception handlers,
    ;; soft single/double ops, FLOAT-OPERATING-MODE plumbing).  Band
    ;; roster: every probed defun is 882-cold or F000-wired in the dist
    ;; (SET-FLOAT-OPERATING-MODE fcell 05->F000386D), so the stock cold
    ;; load contained it; vbin ships.  Without it XR-READ-FLONUM traps
    ;; on SET-FLOAT-OPERATING-MODE's unbound fcell -- reading ANY float
    ;; (1.2, 1.2d0) at the cold REPL errored.  The mode/status
    ;; variables are DEFINE-MAGIC-LOCATIONS cells (dist
    ;; FLOAT-OPERATION-STATUS val 05->F8041138), already built; the 4
    ;; DEFINE-INSTRUCTION-EXCEPTION-HANDLERs ride the same trap-vector
    ;; machinery as ISTACK/LARITH.
    "SYS: I-SYS; FLOAT"
    ;; Boot 43: SYS2; DOUBLE and its cluster sibling SYS2; COMPLEX removed.
    ;; DOUBLE carries an eager top-level (ADD-INITIALIZATION "Make
    ;; *DFLOAT-AND-SCALE-TABLE*" '(setq *dfloat-and-scale-table*
    ;; (make-dfloat-and-scale-table)) '(:once)) (double.lisp:406).  :once is a
    ;; WHEN=FIRST keyword, so ADD-INITIALIZATION EVALs the init form
    ;; IMMEDIATELY at registration (ltop.lisp:363) -- and the deferred MAPC
    ;; registers it pre-banner.  MAKE-DFLOAT-AND-SCALE-TABLE (double.lisp:390)
    ;; calls DFLOAT (sys2/lnumer.lisp:468), which is QLD-warm (dist fcell band
    ;; 0x822, in INNER-SYSTEM-FILE-ALIST, absent from the FSET stub alist) and
    ;; thus unbound in a fresh cold world -> trap 71 pre-banner.  Band oracle:
    ;; DOUBLE is 822:7 (QLD) + 882:1 (only DESCRIBE-DOUBLE noise, cold) -- a
    ;; hygiene-prune candidate, never genuinely cold.  COMPLEX prunes as its
    ;; cluster sibling: 28 defuns, 0 cold-band, 22 QLD, cleanly QLD-band, no
    ;; eager add-initialization.  Both are plain arithmetic files (no flavor
    ;; definitions) so no deferred COMPILE-FLAVOR-METHODS coupling.  See the
    ;; new check-eager-initialization-callees gate (cold-diff.lisp).
    "SYS: SYS; WIRED" "SYS: DEBUGGER; ITRAP-DISPATCH"
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
    ;; Boot 44: SYS: STORAGE; USER-DISK-DRIVER removed (siblings DISK-DRIVER
    ;; and EMBEDDED-DISK-DRIVER kept -- band-audited genuinely cold).
    ;; user-disk-driver.lisp:200 carries a top-level (ADD-INITIALIZATION
    ;; "Initialize user disk" '(initialize-user-disk) '(:system)).  :SYSTEM
    ;; is the SYSTEM-INITIALIZATION-LIST keyword whose DEFAULT-WHEN is FIRST
    ;; (ltop.lisp:303 INITIALIZATION-KEYWORDS), so ADD-INITIALIZATION EVALs
    ;; the init form IMMEDIATELY at registration (ltop.lisp:363-366) -- and
    ;; the deferred MAPC registers it pre-banner.  INITIALIZE-USER-DISK
    ;; (user-disk-driver.lisp:195) calls PROCESS:RESET-LOCK and
    ;; PROCESS:MAKE-LOCK (scheduler/lock-definitions.lisp), both QLD-warm
    ;; defgenerics unbound in a fresh cold world -> trap 71 pre-banner.  The
    ;; boot-43 gate missed it because add-initialization-eager-p only
    ;; matched the literal ONCE/ONCE-ONLY/FIRST/NOW keyword names and never
    ;; classified :SYSTEM's implicit FIRST default (fixed in this boot).
    ;; Band oracle: 42 defuns -> 29 QLD (0x822) + 2 cold-band
    ;; (SIGNAL-DISK-ERROR/SIGNAL-DISK-ERRORS, error-path noise referenced by
    ;; no cold file); no cold file references INITIALIZE-USER-DISK or the
    ;; *user-*-disk-event* vars.  The "Initialize user disk" :system init is
    ;; a post-banner SYSTEM-INITIALIZATION-LIST obligation that must run warm
    ;; (QLD reloads the file with RESET-LOCK/MAKE-LOCK/INITIALIZE-DISK-EVENT
    ;; bound).  Cold DISK-DRIVER.LISP only uses the DISK-EVENT-LOCK accessor
    ;; + *ROOT-DISK-EVENT*, both defined in cold files outside
    ;; user-disk-driver.  See check-eager-initialization-callees
    ;; (cold-diff.lisp).
    "SYS: STORAGE; EMBEDDED-DISK-DRIVER"
    "SYS: IO; LMINI" "SYS: IO; USEFUL-STREAMS"
    "SYS: I-SYS; INTERRUPTS" "SYS: I-SYS; V-INTERRUPTS" "SYS: I-SYS; AUDIO"
    "SYS: EMBEDDING; EMB-BUFFER" "SYS: EMBEDDING; EMB-QUEUE"
    "SYS: EMBEDDING; EMB-MESSAGE-CHANNEL"
    ;; In the original cold set (band oracle: dist GET-SUB-PACKET fcell
    ;; forwards into 05:8820F587) but the .vbin was lost; recompiled in
    ;; the user's Genera 8.5 2026-07-10.  Its MAKE-AREAs create
    ;; NETWORK-CONS-AREA (22) and ETHER-BUFFER-AREA (23) -- the first
    ;; two boot-created areas, ahead of the flavor areas (M3h boot 31).
    "SYS: NETWORK; PKTS"
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
    "SYS: CLCP; LISTFNS" "SYS: CLCP; IOFNS"
    ;; The WHOLE CL sequence layer was cold: the dist band scan over
    ;; #x8821AF00-#x8821C800 (right after ldefsel's helpers) is
    ;; seqfns.lisp's roster verbatim -- MAKE-SEQUENCE, CONCATENATE, MAP,
    ;; SOME/EVERY/NOTANY/NOTEVERY, REDUCE, FILL, REPLACE, the
    ;; SUBSTITUTE/FIND/POSITION/COUNT families.  (Boot 28 read
    ;; LISP:SUBSTITUTE-IF's 0x8235 fcell as "seqfns = QLD", but that was
    ;; a warm PATCH redefining a few dispatchers over the cold file: its
    ;; CLI helpers SUBSTITUTE-TEST et al. stayed in the cold band.  A
    ;; single function's band does not classify its FILE.)  Trap: the
    ;; deferred flavor MAPC's PARSE-DEFFLAVOR calls LISP:NREVERSE
    ;; (defflavor.lisp:557), unbound without this (M3h boot 32); its
    ;; .vbin ships (QLD's INNER-SYSTEM-FILE-ALIST reloads it warm).
    "SYS: CLCP; SEQFNS"
    ;; The CL string layer was cold too: the whole stringfns function
    ;; family is a consecutive 0x882 cold block at #x8821DC54-#x8821E583
    ;; in the dist band scan -- right after seqfns' band and just before
    ;; numerics' (source/band order seqfns,stringfns,numerics).  Trap: the
    ;; deferred flavor MAPC evaluates a cold DEFFLAVOR ->
    ;; ENCODE-FLAVOR-MIXTURE -> FLAVOR-MIXTURE-NAME (flavor/compose.lisp:
    ;; 1284) whose STRING-SEARCH-CHAR / STRING-EQUAL calls (compose.lisp:
    ;; 1289,1302,1304) the optimizer inlines to direct fcell calls of
    ;; CLI:STRING-SEARCH-CHAR-FORWARD / STRING-EQUAL-INTERNAL, defined
    ;; ONLY here -- unbound without this (M3h boot 45, trap 71).  Its
    ;; .vbin ships (QLD's INNER-SYSTEM-FILE-ALIST reloads it warm;
    ;; mini-alists.lisp:78 "Needed by FLAVOR").  Its ADD-OPTIMIZER
    ;; registrations are already covered by *cold-guarded-heads*'
    ;; ADD-OPTIMIZER-INTERNAL.
    "SYS: CLCP; STRINGFNS"
    ;; The CL numeric layer was cold too: the dist band scan puts
    ;; numerics.lisp's roster consecutively in source order at
    ;; #x8821E97A-#x8821EA47 (ISQRT, FLOAT-RADIX/DIGITS/PRECISION/SIGN,
    ;; FLOAT, FLOAT1, FLOAT-OPTIMIZER, FIXNUM-RIGHTMOST-ONE,
    ;; INTEGER-LENGTH) -- right after seqfns' band, matching this
    ;; position.  Trap: the deferred flavor MAPC's handler-table build
    ;; (HANDLER-TABLE-OPTIMAL-N-SLOTS, flavor/handle.lisp:273) calls
    ;; LISP:INTEGER-LENGTH, unbound without this (M3h boot 37); its
    ;; .vbin ships (QLD's INNER-SYSTEM-FILE-ALIST reloads it warm,
    ;; "Needed by TABLE").  Its ADD-OPTIMIZER registration (:125) is
    ;; already covered by *cold-guarded-heads*' ADD-OPTIMIZER-INTERNAL.
    "SYS: CLCP; NUMERICS"))

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

(defparameter *cold-hoisted-deferred-defvars*
  '("*ALL-FLAVOR-NAMES-AARRAY*" "*ALL-GENERIC-FUNCTION-NAMES-AARRAY*")
  "M3h boot 33: deferred defvar inits hoisted to the FRONT of the boot
deferred list.  The flavor completion tables (flavor/global.lisp:129-138)
are MAKE-AARRAY calls -- unevaluable at build time, so they defer -- but
the first deferred DEFFLAVOR-INTERNAL (wired-event-defs.lisp:76, file 2
of the load order) reaches FLAVOR-COMPLETION ->
BOOTSTRAP-FLAVOR-NAMES-AARRAY (defflavor.lisp:1434-1451), which reads
BOTH tables' fill pointers ~3900 forms before flavor/global's own inits
run.  The genuine build had no inversion: Symbolics' generator consed
the aarrays at build time (the dist ships both symbols bound,
DTP-ARRAY).  Modeling MAKE-AARRAY natively is not worth it -- leader
slot 2 holds a PROCESS:MAKE-LOCK instance, which at boot the FSET stub
MAKE-LOCK-COLD (cold-load.lisp:239,449) supplies for free.  Everything
the hoisted forms call is pre-deferred state: MAKE-ARRAY, STRING-APPEND
and CL:RASSOC are cold fdefines, PERMANENT-STORAGE-AREA and
*AARRAY-NAME-ALIST* ship bound.  A head belongs here only if its init
needs NO deferred state and a deferred form EARLIER in load order reads
the variable.")

(defun cold-hoist-deferred-defvars (w)
  "Move the *COLD-HOISTED-DEFERRED-DEFVARS* BOUNDP-guarded init entries
to the front of the emitted deferred list (= tail of the stored reversed
list).  Errors unless every listed name matches exactly one entry: the
hoist list must track the deferrals, not rot.  Returns the count moved."
  (flet ((hoisted-p (entry)
           (let ((sym (cold-deferred-defvar-parts (cdr entry))))
             (and sym (member (vsym-name sym) *cold-hoisted-deferred-defvars*
                              :test #'string=)))))
    (let* ((stored (cold-world-deferred w))
           (moved (remove-if-not #'hoisted-p stored)))
      (unless (= (length moved) (length *cold-hoisted-deferred-defvars*))
        (error "Hoist list wants ~D deferred defvar init~:P, found ~D"
               (length *cold-hoisted-deferred-defvars*) (length moved)))
      (setf (cold-world-deferred w)
            (append (remove-if #'hoisted-p stored) moved))
      (length moved))))

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

(defvar *cold-package-faithful-replay* t
  "When true (the default), cold-finalize wraps each deferred form whose
recording file's package is not SYSTEM-INTERNALS so it replays under that
package.  The boot's deferred MAPC (cold-load.lisp:547) EVALs every form
under the single live *PACKAGE* = SI (cold-load.lisp:565 sets it only
AFTER the MAPC); a form whose evaluation INTERNs -- FLAVOR-MIXTURE-NAME's
bare (INTERN string) regenerating :mixture variant names, compose.lisp:
1296 -- then interns into SI while the form's compile-time-baked symbols
live in the file's package, splitting the flavor across two symbols and
killing FIND-FLAVOR pre-banner (M3h boot 46, useful-streams'
BUFFERED-*-COROUTINE-STREAM).  The wrapper restores the original warm
file-load semantics.  NIL exists for the gate negative test only
\(check-deferred-flavor-composition consults it).")

;; Host-side memos so every wrapper shares ONE materialized prologue per
;; package and ONE epilogue: cold-ref's *cold-object-vmas* cache shares
;; structure by host-object identity, so EQ host forms cost their Qs once.
(defvar *cold-replay-prologues* (make-hash-table :test #'equal))
(defvar *cold-replay-epilogue* nil)

(defun cold-package-replay-entry (entry)
  "Wrap the deferred ENTRY (pkg-string . form) for package-faithful
replay: non-SI forms become
  (PROGN (SETQ PACKAGE (PKG-FIND-PACKAGE \"pkg\")) form
         (SETQ PACKAGE PKG-SYSTEM-INTERNALS-PACKAGE))
mirroring the boot's own idioms verbatim (BUILD-INITIAL-PACKAGES sets
PKG-SYSTEM-INTERNALS-PACKAGE at package.lisp:2388 before the MAPC;
cold-load.lisp:565 is the same SETQ).  A SETQ sandwich, NOT a LET:
*PACKAGE* is DEFVAR-STANDARD whose DEFVAR is explicitly NOT done at cold
load time (package.lisp:87-92), so an interpreted LET could bind it
LEXICALLY and compiled INTERN would read the untouched global cell -- a
silent no-op.  Free interpreted SETQ always writes the value cell, and
ZL:PACKAGE / CL:*PACKAGE* share one cell by replay time
\(permanent-links.lisp:85 via BOOTSTRAP-FORWARD-SYMBOL-CELLS at
cold-load.lisp:545, two lines before the MAPC).  Package OBJECTS cannot
be baked at build time -- BUILD-INITIAL-PACKAGES creates them at first
boot and FIXUP-SYMBOL-PACKAGE swaps the name strings in symbols' package
cells for them -- hence the runtime PKG-FIND-PACKAGE lookup.  Returns
\(values entry wrappedp)."
  (let ((pkg (car entry)))
    (if (or (not *cold-package-faithful-replay*)
            (null pkg)
            (not (stringp pkg))
            (string= pkg "SYSTEM-INTERNALS"))
        (values entry nil)
        (let ((prologue
                (or (gethash pkg *cold-replay-prologues*)
                    (setf (gethash pkg *cold-replay-prologues*)
                          (list (si-vsym "SETQ") (si-vsym "PACKAGE")
                                (list (si-vsym "PKG-FIND-PACKAGE") pkg)))))
              (epilogue
                (or *cold-replay-epilogue*
                    (setf *cold-replay-epilogue*
                          (list (si-vsym "SETQ") (si-vsym "PACKAGE")
                                (si-vsym
                                 "PKG-SYSTEM-INTERNALS-PACKAGE"))))))
          (values
           (cons pkg
                 (list (si-vsym "PROGN") prologue (cdr entry) epilogue))
           t)))))

(defun cold-finalize (w &key reference)
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
6. the storage tables are RE-stamped (cold-fill-storage-tables, M3h boot
   47): every step above ALLOCATES -- deferred-list conses, patch forms,
   strings, and every symbol first referenced by a deferred form (the
   CFM auto-mixture variants exactly) -- so the region free pointers
   cold-build-wired-machinery stamped are stale by now.  A stale fp is
   not cosmetic: BUILD-INITIAL-PACKAGES' FIXUP-SYMBOL-PACKAGE sweep
   walks SYMBOL-AREA only up to the table fp, so late-interned symbols
   never register in their packages (boot 47: the boot-46 package
   wrapper made the mixture INTERN run in CLI, but INTERN still minted
   a fresh CLI twin because the baked variant @801127C3 sat past region
   6's stamped fp #x12151) -- and the boot allocator would treat the
   space past a stale fp as free, consing OVER finalize-baked objects.
   REFERENCE is needed again for the boot-created areas' template rows.
Returns (values deferred-count patch-count package-count)."
  (with-cold-materializer (w)
    (let* ((*cold-load-time-eval* #'cold-operand-eval)
           (package-count
             (cold-load-pkgdcl w (sys-pathname "SYS: SYS; PKGDCL" "lisp")))
           (revived (cold-retry-deferred-defvars w))
           (reconciled (cold-reconcile-linked-defvars w))
           (hoisted (cold-hoist-deferred-defvars w))
           (deferred (reverse (cold-world-deferred w)))
           (store (make-vsym "SYSTEM" "%P-STORE-CONTENTS"))
           (loc-tag (tag 0 (cold-dtp w "LOCATIVE")))
           (add-name (make-vsym "SYSTEM-INTERNALS" "PKG-ADD-RELATIVE-NAME"))
           (patches nil)
           (deferred-qs nil)
           (wrapped 0))
      (flet ((materialize (entry)
               (let ((*cold-default-package* (car entry)))
                 (multiple-value-bind (ft fd)
                     (cold-ref w (cdr entry) :area "WORKING-STORAGE-AREA")
                   (cons ft fd)))))
        ;; Materialize the deferred forms FIRST: load-time-eval constants
        ;; inside them (FIND-GENERIC-FUNCTION-AS-CONSTANT etc., heavy in
        ;; the flavor runtime) mint patches as they materialize.  A new
        ;; DEFERRAL here would never execute -- still a hard error.
        ;; Non-SI forms get the package-faithful replay wrapper (M3h boot
        ;; 46); patches and relative-name forms stay bare -- patch heads
        ;; are the audited value-store class and never INTERN.
        (setf deferred-qs
              (mapcar (lambda (entry)
                        (multiple-value-bind (e wrappedp)
                            (cold-package-replay-entry entry)
                          (when wrappedp (incf wrapped))
                          (materialize e)))
                      deferred))
        (unless (= (length (cold-world-deferred w)) (length deferred))
          (error "~D deferral~:P noted while materializing the deferred list"
                 (- (length (cold-world-deferred w)) (length deferred))))
        ;; Drain patches to a fixed point: materializing a patch entry
        ;; can itself mint another patch (nested veval constants).
        (loop with done = 0
              for round from 0
              for pending = (nthcdr done (reverse (cold-world-patches w)))
              while pending
              do (when (> round 100)
                   (error "Patch materialization did not converge"))
                 (dolist (p pending)
                   (destructuring-bind (vma pkg form) p
                     (let ((base (list store (make-vraw loc-tag vma) form)))
                       ;; Warm-only value heads: no-op pre-banner
                       ;; (*COLD-GUARDED-PATCH-HEADS*, M3h boot 28).
                       (when (and (consp form) (vsym-p (first form))
                                  (member (vsym-name (first form))
                                          *cold-guarded-patch-heads*
                                          :test #'string=))
                         (setf base
                               (list (make-vsym "SYSTEM-INTERNALS" "IF")
                                     (list (make-vsym "SYSTEM-INTERNALS"
                                                      "FBOUNDP")
                                           (list (make-vsym "SYSTEM-INTERNALS"
                                                            "QUOTE")
                                                 (first form)))
                                     base)))
                       (push (materialize (cons pkg base)) patches))
                     (incf done)))
              finally (setf patches (nreverse patches)))
        (unless (= (length (cold-world-deferred w)) (length deferred))
          (error "~D deferral~:P noted while materializing patches"
                 (- (length (cold-world-deferred w)) (length deferred))))
        ;; Boot order: patches first (they fill Qs the deferred forms'
        ;; constants reference), then the deferred forms, then the
        ;; withheld relative names.
        (let ((qs (append
                   patches
                   deferred-qs
                   (mapcar #'materialize
                           (loop for triple in (reverse
                                                (cold-world-relative-names w))
                                 collect (cons "SYSTEM-INTERNALS"
                                               (cons add-name triple)))))))
          (multiple-value-bind (spine-tag spine-data) (cold-nil-q w)
            (dolist (q (reverse qs))
              (setf spine-data (cold-cons w (car q) (cdr q)
                                          spine-tag spine-data)
                    spine-tag (tag 0 (cold-dtp w "LIST"))))
            (cold-set-symbol-value
             w (make-vsym "SYSTEM-INTERNALS" "*COLD-LOAD-DEFERRED-FORMS*")
             spine-tag spine-data))))
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
      (when (plusp hoisted)
        (format t "~&  ~D deferred defvar init~:P hoisted to boot front~%"
                hoisted))
      (when (plusp wrapped)
        (format t "~&  ~D deferred form~:P wrapped for package-faithful ~
replay~%" wrapped))
      ;; LAST: refresh the frontier-derived storage tables (docstring
      ;; point 6).  Nothing after finalize allocates (audits read, emit
      ;; writes pages); check-region-frontier-tables enforces that.
      (cold-fill-storage-tables w :reference reference)
      (values (+ (length patches) (length deferred-qs)
                 (length (cold-world-relative-names w)))
              (length patches) package-count))))

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
    (cold-finalize w :reference reference)))
