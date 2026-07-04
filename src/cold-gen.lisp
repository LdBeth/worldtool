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
    "SYS: GC; IGC-COLD"
    "SYS: I-SYS; WIRED-CONSOLE"
    "SYS: I-SYS; WIRED-SCREEN" "SYS: STORAGE; STORAGE" "SYS: STORAGE; USER-STORAGE"
    "SYS: STORAGE; STACK-WIRING" "SYS: STORAGE; DISK-DRIVER"
    "SYS: STORAGE; USER-DISK-DRIVER" "SYS: STORAGE; EMBEDDED-DISK-DRIVER"
    "SYS: STORAGE; VLM-DISK-UTILITIES" "SYS: IO; LMINI" "SYS: IO; USEFUL-STREAMS"
    "SYS: I-SYS; INTERRUPTS" "SYS: I-SYS; V-INTERRUPTS" "SYS: I-SYS; AUDIO"
    "SYS: EMBEDDING; EMB-BUFFER" "SYS: EMBEDDING; EMB-QUEUE"
    "SYS: EMBEDDING; EMB-MESSAGE-CHANNEL"))
