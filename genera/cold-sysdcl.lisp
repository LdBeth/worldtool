;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;
;;; VLM cold-build system declarations.
;;;
;;; The distribution SYS:SYS;SYSDCL.LISP declares, for each subsystem, the
;;; modules of every machine type (:|3600|, :imach, :vlm).  This partial
;;; LMFS-restored source tree only holds the :vlm-relevant sources, so the
;;; :|3600|/:imach-only modules point at files that do not exist here.  SCT's
;;; Compile System still resolves those module clauses while planning a build
;;; and errors on the missing sources -- even though it would never compile
;;; them on a :VLM world.
;;;
;;; Loading this file REDEFINES the four subsystems that carry cold-load files
;;; (SYSTEM-INTERNALS, GARBAGE-COLLECTOR, ERROR-SYSTEM, LANGUAGE-TOOLS) with
;;; the absent non-:vlm modules dropped, and their :serial/:parallel group
;;; references pruned to match.  Every file named below exists in the tree.
;;; Load this after the base world's sysdcl (so cross-subsystem module
;;; references still resolve), then compile the COLD-SYSTEM defsystem that
;;; worldtool/genera/m2-compile.lisp defines over these four.
;;;
;;; Dropped from SYSTEM-INTERNALS (all :|3600| or :imach-only, sources absent):
;;;   l-arith-defs l-storage-defs l-hardware-defs i-hardware-defs-2
;;;   l-allocate l-cons l-fepio l-prim l-float l-wired-console aux-disk-save
;;;   l-disk-driver imach-disk-driver wired-esrt l-nbs l-console i-console
;;;   l-audio i-hardware-2 i-interrupts l-logging l-auxsb l-fep-channel
;;;   Domino-SCSI-script-compiler Domino-SCSI-script  (+ #+Ivory-Rev-1
;;;   extended-entry, a rev-1-only file).
;;; Dropped from GARBAGE-COLLECTOR: l-defs (lgc), permanent-objects.
;;; Dropped from ERROR-SYSTEM: l-trap (ltrap), l-cometh (lcometh), and
;;;   trap-dispatch-table (:vlm, but machine-generated and absent -- its
;;;   handlers are stubbed on the host side for the cold boot).
;;; Dropped from LANGUAGE-TOOLS: l-compile-only.

(defsystem cold-system
    (:pretty-name "VLM Cold System"
     :default-pathname "sys: sys;"
     :distribute-sources nil
     :distribute-binaries nil
     :source-category :basic)
  (:module components (system-internals
		       garbage-collector
		       error-system
		       language-tools)
	   (:type :system)))

(defsubsystem system-internals
    (:short-name "SI"
     :pretty-name "SI"
     :distribute-sources t
     :distribute-binaries nil
     :default-pathname "sys: sys;"		;So compile-files-of-subsystem works
     :source-category :basic)
  (:module package "sys: sys; pkgdcl"
	   (:type :lisp-read-only)
	   (:root-module nil))
  (:module defs ("sys: io; rddefs"
		 "sys: sys; wired-event-defs"))
  (:module i-arith-defs ("sys: sys; iarithdefs")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-sysdef ("sys: i-sys; sysdef" "sys: i-sys; sysdf1")
	   (:package system)
	   (:machine-types (:imach :vlm)))
  (:module i-storage-defs ("sys: storage; I-storage-defs")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-hardware-defs-1 ("sys: sys2; bars"
			      "sys: storage; disk-definitions")
	   (:machine-types (:imach :vlm)))
  (:module-group l-defs
   (:parallel
     "sys: sys2; macro"
     i-arith-defs
     "sys: sys2; bigdefs"
     i-sysdef
     i-storage-defs
     "sys: sys2; lnumer-defs"
     i-hardware-defs-1))
  (:module metering-defs ("sys: metering; metering-definitions"
			  "sys: metering; metering-cold"
			  "sys: metering; metering-macros")
	   (:source-category :optional))
  (:module i-block-functions ("sys: i-sys; block-functions") (:machine-types (:imach :vlm)))
  (:module-group main
   (:parallel
     "sys: sys; aarray"
     "sys: sys2; advise"
     i-block-functions
     "sys: sys2; character-sets"
     "sys: sys2; character-styles"
     "sys: sys; cold-load"
     "sys: sys; command-loop"
     "sys: sys; console"
     "sys: sys; expand-do"
     "sys: io; dribbl"
     "sys: sys2; encaps"
     "sys: sys; eval"
     "sys: io; format"
     "sys: sys; fspec"
     "sys: sys2; hash"				;this uses macros from SYS:FLAVOR;
     "sys: sys2; hash-compatibility"
     "sys: sys2; heap"
     "sys: io; indenting-stream"
     "sys: io; interactive-stream"
     "sys: io; input-editor"
     "sys: io; iterators"
     "sys: sys2; let"
     "sys: sys; lisp-syntax"
     "sys: sys; lispfn"
     "sys: sys2; login"
     "sys: sys; ltop"
     "sys: sys2; maclsp"
     "sys: sys; macroexpand"
     "sys: sys2; memory-cold"
     "sys: sys; mini-alists"
     "sys: sys2; numer"
     "sys: embedding; rpc; octet-structure-runtime"
     "sys: sys; package"
     "sys: sys; packerr"
     "sys: sys2; plane"
     "sys: io; print"
     "sys: io; qio"
     "sys: io; read"
     "sys: io; readers"
     "sys: sys2; resour"
     "sys: sys2; selev"
     "sys: sys; sort"
     "sys: sys; standard-values"
     "sys: sys2; storage-categories"
     "sys: io; stream"
     "sys: sys2; string"
     "sys: sys2; struct-cold"
     "sys: sys; sysdcl"				;necessary for cold-loading to win
     "sys: io; unix-translating-streams"
     "sys: sys; wired-event-log")		;nothing in here is l-specific
   (:in-order-to :load (:load metering-defs))
   )
  (:module readtable-compiler "sys: io; rtc")
  (:module readtable ("sys: io; rdtbl"
		      "sys: clcp; readtable"
		      "sys: clcp; ansi-readtable"
		      "sys: embedding; rpc; c-readtable")
	   (:type :readtable)
	   (:in-order-to :compile (:load readtable-compiler)))
  (:module i-allocate ("sys: sys; i-allocate")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-cons ("sys: sys; icons")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-fepio ("sys: sys; ifepio")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-prim ("sys: sys; iprim"
		   "sys: sys; istack")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-float ("sys: i-sys; float")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module i-wired-console ("sys: i-sys; wired-console" "sys: i-sys; wired-screen")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module stack-wiring ("sys: storage; stack-wiring")
	   (:machine-types (:imach :vlm)) (:source-category :optional))
  (:module ivory-disk-driver ("sys: storage; disk-driver"
			      "sys: storage; user-disk-driver"
			      "sys: storage; embedded-disk-driver")
	   (:machine-types (:IMach :VLM)) (:source-category :optional))
  (:module vlm-disk-driver ("sys: storage; vlm-disk-utilities")
		    (:machine-types (:VLM)) (:source-category :optional))
  (:module v-console ("sys: i-sys; console-stubs") (:machine-types (:vlm)))
  (:module i-hardware-1 ("sys: i-sys; audio")
	   (:machine-types (:imach :vlm)))
  (:module v-hardware-2 ("sys: i-sys; v-clock")
	   (:machine-types (:vlm)))
  (:module i-interrupts-base ("sys: i-sys; interrupts") (:machine-types (:imach :vlm)))
  (:module v-interrupts ("sys: i-sys; v-interrupts") (:machine-types (:vlm)))
  (:module-group l-main
   (:parallel
     "sys: sys; ldata"				;putting this in defs is not a good idea,
						;because loading it really screws things up
     "sys: sys2; ldefsel"
     "sys: sys; lcode"
     i-allocate
     "sys: sys; allocate-common"
     i-cons
     "sys: sys; objects"
     "sys: sys; describe"
     "sys: sys; cold-load-stream"
     i-fepio
     i-prim
     "sys: sys; larith"
     "sys: sys; division"
     "sys: sys2; lnumer"
     i-float
     "sys: sys2; bignum"
     "sys: sys2; double"
     "sys: sys2; complex"
     "sys: sys2; rat"
     "sys: sys; eql-dispatch"
     "sys: sys; wired"
     i-wired-console
     "sys: storage; storage"
     "sys: storage; user-storage"
     stack-wiring
     (:serial ivory-disk-driver vlm-disk-driver)
     ;; "sys: io; lmini"
     v-console
     "sys: io; useful-streams"
     i-interrupts-base v-interrupts
     i-hardware-1 v-hardware-2)
   (:source-category (:optional
		       (:basic "sys: sys2; ldefsel"
			       "sys: sys; cold-load-stream"
			       "sys: sys; describe"
			       "sys: io; useful-streams")))))

(defsubsystem garbage-collector
    (:default-pathname "sys: gc;"
     :distribute-sources t
     :distribute-binaries nil
     :source-category :optional)
  (:module defs ((system-internals l-defs))
	   (:root-module nil))
  (:module gc-defs ("gc-defs")
	   (:source-category :basic))
  (:module i-defs ("igc" "igc-cold") (:machine-types (:imach :vlm)))
  (:module machine-independent ("gc")
	   (:in-order-to :compile (:load defs gc-defs i-defs))
	   (:in-order-to :load (:load gc-defs i-defs)))
  (:module other ("sys:gc;full-gc" "sys:gc;reorder-memory" "sys:gc;debug-info")
	   (:in-order-to :compile (:load defs gc-defs i-defs machine-independent))
	   (:in-order-to :load (:load gc-defs i-defs machine-independent)))
  (:module in-place ("gc-in-place")))

(defsubsystem error-system
    (:default-pathname "sys: debugger;"
     :distribute-sources t
     :distribute-binaries nil
     :source-category :basic)
  (:module error-table-compiler
	   ("error-table-compiler" "error-table-expanders")
	   (:machine-types (:imach :vlm))
	   (:root-module nil))
  (:module i-trap ("itrap-defs" "itrap-dispatch" "itrap") (:machine-types (:imach :vlm)))
  (:module-group trap (:serial i-trap "trap"))
  (:module i-cometh ("icometh") (:machine-types (:imach :vlm)))
  (:serial "error-system-defs"
	   (:parallel "handlers" "condition-support")
	   "frame-support"
	   "mini-debugger"
	   "condition"
	   "syscond"
	   trap
	   i-cometh
	   "cometh"
	   "ansi-conditions"))

(defsubsystem language-tools
    (:default-pathname "sys:clcp;"
     :distribute-sources t
     :distribute-binaries nil
     :source-category :basic)
  (:module examples "mapforms-examples" (:root-module nil))
  (:module i-compile-only ("sys: i-sys; compile-only") (:machine-types (:imach :vlm)))
  (:serial
    "sys; lisp-database-cold" "sys; lisp-database"
    "mapforms" "annotate" "subst" "setf" "setf-install" "lambda-list"
    i-compile-only))
