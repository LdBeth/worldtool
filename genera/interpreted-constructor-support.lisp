;;; -*- Package:SYSTEM-INTERNALS; Mode:LISP; Base:8 -*-
;;;
;;; Shadow-cursor protocol for INTERPRETED flavor constructors.
;;; Not part of Genera: written for the linux-vlm from-scratch cold world
;;; (worldtool), compiled on the OG2 VLM world (m2 flow), loaded cold.
;;;
;;; Why this file exists (QLD attempts 13-15 of the fresh-world bootstrap):
;;;
;;; A from-scratch world composes its flavors in fresh source order, so
;;; VALIDATE-CONSTRUCTOR-FUNCTIONS rejects the vbin-dumped constructors
;;; (their CONSTRUCTOR-DERIVATION embeds the Symbolics build world's
;;; historical instance-variable slot order) and regenerates them; with no
;;; compiler loaded, COMPILE-FUNCTION-LIST is (MAPC #'EVAL forms), so the
;;; regenerated constructors run INTERPRETED -- a path stock Genera never
;;; exercises, because Symbolics' own worlds inherit the historical flavor
;;; structures and always pass validation.
;;;
;;; The generated constructor body assumes the compiled-code contract:
;;; %MAKE-STRUCTURE leaves BAR-1 at the first instance-variable slot and
;;; consecutive (%BLOCK-WRITE 1 ...) forms consume it.  Interpreted, that
;;; contract is unsound on real Ivory: EVERY allocation instruction
;;; (%ALLOCATE-LIST-BLOCK / %ALLOCATE-STRUCTURE-BLOCK) caches the fresh
;;; block address in BAR-1, and the interpreter allocates between the
;;; constructor's calls (macroexpanding each (SI:%BLOCK-WRITE 1 v) form
;;; conses, for a start).  The slot writes scatter into freshly consed
;;; cells, the instance tail stays DTP-NULL, and the first method to read
;;; an instance variable takes error trap 71 (observed live: PC 4 of
;;; (FLAVOR:METHOD MAKE-INSTANCE PROCESS), the DEBUG-FLAG read, QLD
;;; attempt 15).  The .BAR. save/restore idiom the digester emits around
;;; hard init forms cannot help: it guards the FORM's clobber, not the
;;; interpreter's own consing.
;;;
;;; The fix: keep the block-write cursor in special variables -- storage
;;; the interpreter's consing cannot touch -- maintained entirely inside
;;; COMPILED bodies (this file), where no allocation can intervene between
;;; reading a BAR and saving it.  The worldtool graft repoints the
;;; function cells of %MAKE-STRUCTURE, %BLOCK-1/2/3-WRITE, and
;;; %READ/%WRITE-INTERNAL-REGISTER at the functions below (saving the
;;; original definitions under the -PRIMITIVE names this file funcalls),
;;; so the digested constructor's protocol works unchanged:
;;;
;;;   - (%MAKE-STRUCTURE ...) primes the cursor from BAR-1 before any
;;;     interpreter consing can clobber it;
;;;   - (%BLOCK-1-WRITE v) restores BAR-1 from the cursor, does the raw
;;;     tag-blind write (DTP-NULL values included -- unbound slots are
;;;     written as (%SET-TAG 'VAR DTP-NULL) markers), and saves the
;;;     post-incremented BAR-1 back;
;;;   - the .BAR. idiom's (%READ/%WRITE-INTERNAL-REGISTER ... %REGISTER-
;;;     BAR-n) function calls are redirected to the cursor variables, so
;;;     saving and restoring around hard init forms protects the CURSOR --
;;;     which makes even a nested interpreted constructor inside a hard
;;;     init form compose correctly.  (Interpreted reads of the real BARs
;;;     were meaningless anyway: any consing invalidates them.)
;;;
;;; Fcell indirection only: compiled code open-codes all of these as
;;; instructions and never sees this file.  Both defining files of the
;;; originals (sys;icons for %MAKE-STRUCTURE, sys;iprim for the register
;;; functions) are cold-only, never QLD-reloaded, so the redirects
;;; survive QLD; once the compiler is loaded, constructors are compiled
;;; again and this protocol goes quiescent.

(DEFVAR *INTERPRETED-BAR-1* 0
  "Shadow of BAR-1 for interpreted constructor block writes.")
(DEFVAR *INTERPRETED-BAR-2* 0
  "Shadow of BAR-2 for interpreted code's block-register protocol.")
(DEFVAR *INTERPRETED-BAR-3* 0
  "Shadow of BAR-3 for interpreted code's block-register protocol.")

;;; The worldtool graft binds these to the original definitions before
;;; repointing the standard names at the wrappers below.
;;;   %MAKE-STRUCTURE-PRIMITIVE            <- sys;icons %MAKE-STRUCTURE
;;;   %READ-INTERNAL-REGISTER-PRIMITIVE    <- sys;iprim %READ-INTERNAL-REGISTER
;;;   %WRITE-INTERNAL-REGISTER-PRIMITIVE   <- sys;iprim %WRITE-INTERNAL-REGISTER

(DEFUN %INTERPRETED-MAKE-STRUCTURE (POINTER-DTP HEADER-DTP HEADER-TYPE
				    REST-OF-HEADER AREA LENGTH)
  "%MAKE-STRUCTURE, then capture BAR-1 (left at the first data slot) into
*INTERPRETED-BAR-1* before returning to the interpreter, whose first cons
would clobber it."
  (PROG1 (FUNCALL #'%MAKE-STRUCTURE-PRIMITIVE POINTER-DTP HEADER-DTP HEADER-TYPE
		  REST-OF-HEADER AREA LENGTH)
	 (SETQ *INTERPRETED-BAR-1* (%READ-INTERNAL-REGISTER %REGISTER-BAR-1))))

(DEFUN %INTERPRETED-BLOCK-1-WRITE (VALUE)
  "Write VALUE (any tag, DTP-NULL included) at the shadow cursor and
advance it.  The real BAR-1 is only live inside this compiled body."
  (%WRITE-INTERNAL-REGISTER *INTERPRETED-BAR-1* %REGISTER-BAR-1)
  (%BLOCK-WRITE 1 VALUE)
  (SETQ *INTERPRETED-BAR-1* (%READ-INTERNAL-REGISTER %REGISTER-BAR-1))
  NIL)

(DEFUN %INTERPRETED-BLOCK-2-WRITE (VALUE)
  (%WRITE-INTERNAL-REGISTER *INTERPRETED-BAR-2* %REGISTER-BAR-2)
  (%BLOCK-WRITE 2 VALUE)
  (SETQ *INTERPRETED-BAR-2* (%READ-INTERNAL-REGISTER %REGISTER-BAR-2))
  NIL)

(DEFUN %INTERPRETED-BLOCK-3-WRITE (VALUE)
  (%WRITE-INTERNAL-REGISTER *INTERPRETED-BAR-3* %REGISTER-BAR-3)
  (%BLOCK-WRITE 3 VALUE)
  (SETQ *INTERPRETED-BAR-3* (%READ-INTERNAL-REGISTER %REGISTER-BAR-3))
  NIL)

(DEFUN %INTERPRETED-READ-INTERNAL-REGISTER (REGISTER)
  "BAR registers read from interpreted code come from the shadows; the
real ones are dead the moment the interpreter conses.  Everything else
goes to the original."
  (COND ((= REGISTER %REGISTER-BAR-1) *INTERPRETED-BAR-1*)
	((= REGISTER %REGISTER-BAR-2) *INTERPRETED-BAR-2*)
	((= REGISTER %REGISTER-BAR-3) *INTERPRETED-BAR-3*)
	(T (FUNCALL #'%READ-INTERNAL-REGISTER-PRIMITIVE REGISTER))))

(DEFUN %INTERPRETED-WRITE-INTERNAL-REGISTER (VALUE REGISTER)
  (COND ((= REGISTER %REGISTER-BAR-1) (SETQ *INTERPRETED-BAR-1* VALUE))
	((= REGISTER %REGISTER-BAR-2) (SETQ *INTERPRETED-BAR-2* VALUE))
	((= REGISTER %REGISTER-BAR-3) (SETQ *INTERPRETED-BAR-3* VALUE))
	(T (FUNCALL #'%WRITE-INTERNAL-REGISTER-PRIMITIVE VALUE REGISTER)))
  VALUE)
