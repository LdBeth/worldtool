;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; M2 step 1: export the cold-load manifest and compile the bootstrap .vbins.
;;; Run in the OG2 VLM Genera world (Load File or paste into a Lisp Listener).
;;; compile-file on a VLM emits .vbin natively -- no cross-compilation involved.
;;;
;;; Produces:
;;;   *manifest-output*  -- one line per file the running world's cold load
;;;                         contained (SI:*COLD-LOADED-FILE-PROPERTY-LISTS*).
;;;                         This is the authoritative M3 file set.
;;;   .vbin files next to their sources for (a) every cold-load manifest file
;;;   and (b) MINI-ALISTS plus every file in the four QLD alists
;;;   (INNER-SYSTEM, REST-OF-PATHNAMES, CHAOS, SYSTEM-SYSTEM).

(defparameter *manifest-output* "SYS:SITE;COLD-LOAD-MANIFEST.TEXT"
  "Where to write the cold-load manifest.  Edit if SYS:SITE; is not writable.")

(defparameter *loaded-files-output* "SYS:SITE;LOADED-FILES.TEXT"
  "Where M2-DUMP-LOADED-FILES writes its records.")

(defvar *m2-failures* nil "(pathname . condition-report) for compiles that failed.")
(defvar *m2-compiled* nil "Source pathnames successfully compiled this run.")

(defun m2-source-pathname (name)
  "Parse NAME (a namestring or pathname) and return its .LISP source, newest."
  (let ((path (fs:parse-pathname (string name))))
    (send path :new-pathname :canonical-type :lisp :version :newest)))

(defun m2-compile-one (name)
  (let ((src (m2-source-pathname name)))
    (unless (member src *m2-compiled* :test #'equalp)
      (format t "~&Compiling ~A ..." src)
      (scl:condition-case (err)
	   (progn (compile-file src)
		  (push src *m2-compiled*))
	 (error
	   (format t "~&*** FAILED ~A: ~A" src (dbg:report-string err))
	   (push (cons src (dbg:report-string err)) *m2-failures*))))))

(defun m2-dump-manifest ()
  ;; NOTE: in a fully built world this list is always empty --
  ;; FS:CANONICALIZE-COLD-LOAD-PATHNAMES moves the records onto the generic
  ;; pathnames' property lists and then clears the variable (io/logpath.lisp).
  ;; Use M2-DUMP-LOADED-FILES instead to recover the bootstrap file set.
  (cond ((not (boundp 'si:*cold-loaded-file-property-lists*))
	 (format t "~&*** SI:*COLD-LOADED-FILE-PROPERTY-LISTS* is unbound in this world!"))
	(t
	 (with-open-file (s *manifest-output* :direction :output)
	   (dolist (elem si:*cold-loaded-file-property-lists*)
	     (format s "~A~%" (first elem))))
	 (format t "~&Wrote ~D manifest entries to ~A"
		 (length si:*cold-loaded-file-property-lists*) *manifest-output*))))

(defun m2-dump-loaded-files ()
  "Dump every file this world remembers loading, with its load records.
Walks the interned-pathname tables of every host and reports each pathname
carrying a :FILE-ID-PACKAGE-ALIST property.  Each record line shows the
package it was loaded into and the truename + creation date of the binary
actually loaded (cold-load and MINI-loaded files keep the pathname of the
machine they were built on).  The cold-load file set is recovered from this
on the host side by subtracting the QLD alists."
  (let ((records nil))
    (dolist (host (append fs:*pathname-host-list* fs:*logical-pathname-host-list*))
      (let ((table (send host :pathname-hash-table nil)))
	(when table
	  (maphash #'(lambda (ignore path)
		       (let ((alist (send path :get :file-id-package-alist)))
			 (when (and alist (not (member path records :key #'first)))
			   (push (list path alist) records))))
		   table))))
    (with-open-file (s *loaded-files-output* :direction :output)
      (dolist (record records)
	(format s "~&~A" (send (first record) :string-for-printing))
	(dolist (entry (second record))
	  (format s "~&  ~S" entry))))
    (format t "~&Wrote ~D load records to ~A" (length records) *loaded-files-output*)))

(defun m2-compile-manifest ()
  "Compile every file the cold load contained."
  (dolist (elem si:*cold-loaded-file-property-lists*)
    (m2-compile-one (first elem))))

(defun m2-compile-alists ()
  "Compile MINI-ALISTS itself plus every binary file in the four QLD alists."
  (m2-compile-one "SYS: SYS; MINI-ALISTS")
  (dolist (alist (list si:inner-system-file-alist
		       si:rest-of-pathnames-file-alist
		       si:chaos-file-alist
		       si:system-system-file-alist))
    (dolist (entry alist)
      ;; entry = (mini-namestring package binary-p force-base); skip text files
      (when (third entry)
	(m2-compile-one (first entry))))))

;;; The SI-subsystem files (sysdcl.lisp, machine-type :VLM resolved) that the
;;; alist pass did NOT cover -- the cold-load kernel candidates.  Derived by
;;; crossing sysdcl.lisp against the world's load records (loaded-files.text)
;;; and the .vbins already present.  SYS:SYS;PKGDCL is deliberately absent:
;;; it is a :lisp-read-only module, read as source by the cold-load generator.
(defparameter *cold-set-plain-files*
  '("SYS: IO; RDDEFS" "SYS: SYS; WIRED-EVENT-DEFS" "SYS: SYS; IARITHDEFS"
    "SYS: I-SYS; SYSDEF" "SYS: I-SYS; SYSDF1" "SYS: SYS2; BARS"
    "SYS: STORAGE; DISK-DEFINITIONS" "SYS: SYS2; BIGDEFS" "SYS: SYS2; LNUMER-DEFS"
    "SYS: METERING; METERING-COLD" "SYS: METERING; METERING-MACROS"
    "SYS: I-SYS; BLOCK-FUNCTIONS" "SYS: SYS; AARRAY" "SYS: SYS2; ADVISE"
    "SYS: SYS; COLD-LOAD" "SYS: SYS; COMMAND-LOOP" "SYS: SYS; EXPAND-DO"
    "SYS: IO; DRIBBL" "SYS: SYS2; ENCAPS" "SYS: SYS; EVAL" "SYS: SYS; FSPEC"
    ;; Boot 38: HASH/HEAP/INTERACTIVE-STREAM/STANDARD-VALUES/
    ;; VLM-DISK-UTILITIES removed (band-audit-proven QLD files with fatal
    ;; pre-banner deferred CFMs); IO; STREAM added (genuinely cold).  See
    ;; cold-gen.lisp *cold-load-order* + coldset-audit.lisp.
    ;; Boot 39: INPUT-EDITOR removed too (interactive-stream's cluster
    ;; sibling, band-proven 0x8223 QLD; its DEFUN-IN-FLAVOR/method fdefines
    ;; and PRINTING-INPUT-EDITOR CFM all target the pruned INTERACTIVE-STREAM
    ;; flavor -> fatal pre-banner FIND-FLAVOR error / composition WARN).
    "SYS: IO; ITERATORS" "SYS: SYS2; LET"
    "SYS: SYS; LISPFN" "SYS: SYS; LTOP" "SYS: SYS2; MACLSP"
    "SYS: SYS; MACROEXPAND" "SYS: SYS2; MEMORY-COLD"
    "SYS: EMBEDDING; RPC; OCTET-STRUCTURE-RUNTIME" "SYS: SYS; PACKAGE"
    "SYS: SYS2; PLANE" "SYS: IO; PRINT" "SYS: IO; QIO" "SYS: IO; READ"
    "SYS: IO; READERS" "SYS: SYS2; RESOUR" "SYS: SYS2; SELEV" "SYS: SYS; SORT"
    "SYS: SYS2; STORAGE-CATEGORIES" "SYS: IO; STREAM"
    "SYS: SYS2; STRING" "SYS: SYS2; STRUCT-COLD"
    "SYS: IO; UNIX-TRANSLATING-STREAMS" "SYS: SYS; WIRED-EVENT-LOG"
    "SYS: IO; RTC" "SYS: SYS; LDATA" "SYS: SYS; LCODE" "SYS: SYS; I-ALLOCATE"
    "SYS: SYS; ALLOCATE-COMMON" "SYS: SYS; ICONS" "SYS: SYS; OBJECTS"
    "SYS: SYS; DESCRIBE" "SYS: SYS; COLD-LOAD-STREAM" "SYS: SYS; IFEPIO"
    "SYS: SYS; IPRIM" "SYS: SYS; ISTACK" "SYS: SYS; LARITH" "SYS: SYS2; DOUBLE"
    "SYS: SYS2; COMPLEX" "SYS: SYS; WIRED" "SYS: I-SYS; WIRED-CONSOLE"
    "SYS: I-SYS; WIRED-SCREEN" "SYS: STORAGE; STORAGE" "SYS: STORAGE; USER-STORAGE"
    "SYS: STORAGE; STACK-WIRING" "SYS: STORAGE; DISK-DRIVER"
    "SYS: STORAGE; USER-DISK-DRIVER" "SYS: STORAGE; EMBEDDED-DISK-DRIVER"
    "SYS: IO; LMINI" "SYS: IO; USEFUL-STREAMS"
    "SYS: I-SYS; INTERRUPTS" "SYS: I-SYS; V-INTERRUPTS" "SYS: I-SYS; AUDIO"
    "SYS: EMBEDDING; EMB-BUFFER" "SYS: EMBEDDING; EMB-QUEUE"
    "SYS: EMBEDDING; EMB-MESSAGE-CHANNEL"))

;;; :readtable-type modules: SCT compiles these with SI:RTC-FILE, not
;;; COMPILE-FILE (sct/module-types.lisp, readtable-compile-driver).
(defparameter *cold-set-readtable-files*
  '("SYS: IO; RDTBL" "SYS: CLCP; READTABLE" "SYS: CLCP; ANSI-READTABLE"
    "SYS: EMBEDDING; RPC; C-READTABLE"))

;;; Cold-load files OUTSIDE the SI subsystem that the original crossing
;;; missed (found M3d, 2026-07-04, via loaded-files.text records + source
;;; markers): LISP-DATABASE-COLD is language-tools' cold half (defines
;;; PROCLAIM, SI:DEFVAR-1, LT::DEFVAR-1-INTERNAL-1 -- "This file is in the
;;; cold load"); ITRAP-DISPATCH carries the trap handlers, including the
;;; UNEXPECTED-TRAP-HANDLER catch-all that fills every trap vector the
;;; cold set doesn't set explicitly (the 26:F8048DBB filler in the
;;; distribution world's trap page).
(defparameter *cold-set-late-found-files*
  '("SYS: SYS; LISP-DATABASE-COLD" "SYS: DEBUGGER; ITRAP-DISPATCH"))

(defun m2-compile-cold-set ()
  "Compile the SI-subsystem cold-load candidates the alist pass missed."
  (dolist (f *cold-set-plain-files*)
    (m2-compile-one f))
  (dolist (f *cold-set-late-found-files*)
    (m2-compile-one f))
  (dolist (f *cold-set-readtable-files*)
    (let ((src (m2-source-pathname f)))
      (format t "~&RTC-compiling ~A ..." src)
      (scl:condition-case (err)
	   (progn (si:rtc-file src)
		  (push src *m2-compiled*))
	 (error
	   (format t "~&*** FAILED ~A: ~A" src (dbg:report-string err))
	   (push (cons src (dbg:report-string err)) *m2-failures*))))))

;;; SYS:IO;LMINI contains a top-level (REMEMBER-ACCESS-PATH), a compile-time
;;; macro that bakes the chaos address of the host holding the source file
;;; into the binary (MINI-DESTINATION-ADDRESS / MINI-ROUTING-ADDRESS) so the
;;; cold load knows which file server to MINI-load from.  When the source
;;; host has no chaos address in the namespace the expansion dies with
;;; (LDB (BYTE 10 10) NIL).  The baked address only matters if a fresh cold
;;; load really MINI-loads over Chaosnet -- our generator replaces that --
;;; so compile a patched copy that substitutes a fixed address instead.

(defparameter *m2-lmini-chaos-address* #o401
  "Chaos address (subnet 1, host 1) baked into the patched LMINI.
Only reachable if a fresh cold load actually MINI-loads over Chaosnet.")

(defun m2-compile-lmini ()
  "Compile SYS:IO;LMINI with (REMEMBER-ACCESS-PATH) replaced by literal setqs.
The patched copy is written as a new version of the source file itself (the
old version stays below it), so the .vbin lands in SYS:IO; and the wildcard
.vbin copy picks it up.  Rerunning on an already-patched newest version
reports nothing to do."
  (let ((src (m2-source-pathname "SYS: IO; LMINI"))
	(patched nil))
    (with-open-file (in src)
      (with-open-file (out src :direction :output)
	(loop for line = (read-line in nil nil)
	      while line
	      do (cond ((string-equal (string-trim '(#\Space #\Tab) line)
				      "(remember-access-path)")
			(format out "(setq mini-destination-address ~O~@
				       mini-routing-address ~O)~%"
				*m2-lmini-chaos-address* *m2-lmini-chaos-address*)
			(setq patched t))
		       (t (write-line line out))))))
    (cond ((not patched)
	   (format t "~&*** (remember-access-path) not found in ~A; nothing compiled~@
			(already patched?  the newest version has no macro call)" src))
	  (t
	   (scl:condition-case (err)
		(let ((bin (compile-file src)))
		  (push src *m2-compiled*)
		  (setq *m2-failures*
			(delete src *m2-failures* :key #'car :test #'equalp))
		  (format t "~&Patched LMINI compiled to ~A." bin))
	      (error
		(format t "~&*** FAILED ~A: ~A" src (dbg:report-string err))
		(push (cons src (dbg:report-string err)) *m2-failures*)))))))

(defun m2-report ()
  (format t "~2&M2: ~D files compiled, ~D failures."
	  (length *m2-compiled*) (length *m2-failures*))
  (dolist (f (reverse *m2-failures*))
    (format t "~&  FAILED ~A: ~A" (car f) (cdr f))))

(defun m2-run ()
  (setq *m2-failures* nil *m2-compiled* nil)
  (m2-dump-manifest)
  (m2-compile-manifest)
  (m2-compile-alists)
  (m2-report))

(format t "~&M2 script loaded.  Steps:
  (cl-user::m2-dump-loaded-files) ; all load records -> *loaded-files-output* [done]
  (cl-user::m2-compile-alists)    ; MINI/QLD file sets -> .vbin               [done]
  (cl-user::m2-compile-cold-set)  ; SI cold-load kernel -> .vbin              [done]
  (cl-user::m2-compile-lmini)     ; patched LMINI (chaos-address workaround)  <- next
  (cl-user::m2-report)~%")
