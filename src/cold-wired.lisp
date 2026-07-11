;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: the architecturally-fixed wired furniture.
;;;
;;; M3a scope: trap-vector page (every slot filled with a catch-all even-pc
;;; -- the emulator halts with IllegalTrapVector on anything else), zeroed
;;; FEP/SYSCOM communication pages, and the hand-built NIL/T page.
;;;
;;; M3e adds the architectural region set (numbered exactly like the
;;; distribution world: FEP / wired / safeguarded / control-stack /
;;; binding-stack / stack) and reserves the wired region tables and the
;;; SAFEGUARDED storage tables at their ground-truth addresses -- both
;;; blocks are the generator's first allocations in their regions, so the
;;; addresses are deterministic:
;;;   #xF804120D  *REGION-FREE-POINTER*        1024 ART-Q
;;;   #xF804160E  *REGION-GC-POINTER*          1024 ART-Q
;;;   #xF8041A0F  *REGION-QUANTUM-ORIGIN*      1024 ART-Q
;;;   #xF8041E10  *REGION-QUANTUM-LENGTH*      1024 ART-Q
;;;   #xF8042211  *REGION-BITS*                1024 ART-Q
;;;   #xF0000002  *AREA-NAME*                   128 ART-Q-LIST, fp leader
;;;   #xF0000085  *AREA-MAXIMUM-QUANTUM-SIZE*   128 ART-Q-LIST, fp leader
;;;   #xF0000108  *AREA-REGION-QUANTUM-SIZE*    128 ART-Q-LIST, fp leader
;;;   #xF000018B  *AREA-REGION-LIST*            128 ART-16B,    fp leader
;;;   #xF00001CE  *AREA-REGION-BITS*            128 ART-Q-LIST, fp leader
;;;   #xF000024F  *REGION-FREE-POINTER-BEFORE-FLIP*  1024 ART-Q
;;;   #xF0000650  *REGION-LIST-THREAD*         1024 ART-16B
;;;   #xF0000851  *REGION-CREATED-PAGES*       1024 ART-Q
;;;   #xF0000C52  *REGION-AREA*                1024 ART-16B
;;;   #xF0000E53  *OBLAST-FREE-SIZE*           2048 ART-8B  (ends #xF0001054)
;;; Contents are filled after the cold load by cold-machinery.lisp.

(in-package #:worldtool)

;;; End of the wired VMA=PMA zone we may allocate into.  Ground truth ends
;;; at #xF804A600 (SYSCOM %WIRED-VIRTUAL-ADDRESS-HIGH); staying inside it
;;; keeps the fresh world's wired footprint comparable.  The wired region
;;; itself is a full quantum (its table entry says 1 quantum, like the
;;; distribution's region 1); the machinery gate asserts the free pointer
;;; stayed under this limit.
(defconstant +wired-zone-limit+ #xF804A600)

(defun cold-init-architectural-regions (w)
  "Regions 0-5, numbered exactly like the distribution world.
FEP-AREA is the vma=pma image of the FEP reservation: fully allocated,
no pages in the world file.  STACK-AREA's initial region is reserved
address space only (the first data stack is created at first boot by
GROW-DATA-STACK, allocate-common.lisp:990)."
  ;; Reps mirror bit 0 of *cold-architectural-region-bits*; only the two
  ;; structure regions ever see COLD-ALLOC.
  (let ((fep (cold-add-region w "FEP-AREA" #xF8000000 #x40000 :rep :list)))
    (setf (cold-region-free fep) #xF8040000))
  (cold-add-region w "WIRED-CONTROL-TABLES" #xF8040000 #x10000)
  (cold-add-region w "SAFEGUARDED-OBJECTS-AREA" #xF0000000 #x10000)
  (cold-add-region w "CONTROL-STACK-AREA" #xF6000000 #x10000 :rep :list)
  (cold-add-region w "BINDING-STACK-AREA" #xF2000000 #x10000 :rep :list)
  (cold-add-region w "STACK-AREA" #xF0010000 #x20000 :rep :list))

(defun cold-build-symbol-block (w vma &key value-tag value-data)
  "The fixed 5-Q symbol shape (NIL and T; SYMBOL-AREA symbols reuse it):
header-p pname / value cell / dtp-null self (unbound function) / plist /
package.  Pname, plist and package cells start as placeholders."
  (let ((dtp-header-p (cold-dtp w "HEADER-P"))
        (dtp-null (cold-dtp w "NULL")))
    (cw-set w vma (tag 0 dtp-header-p) 0)          ; pname (M3b fixup)
    (cw-set w (+ vma 1) value-tag value-data)      ; value cell
    (cw-set w (+ vma 2) (tag 0 dtp-null) vma)      ; unbound function cell
    (cw-set w (+ vma 3) 0 0)                       ; plist (M3b fixup)
    (cw-set w (+ vma 4) 0 0)))                     ; package (M3b fixup)

(defun cold-nil-q (w)
  "The Q representing NIL as a value."
  (values (tag 0 (cold-dtp w "NIL")) (cold-world-nil-vma w)))

(defun cold-stamp-nil-t (w)
  "Fill the NIL/T cells COLD-BUILD-SYMBOL-BLOCK left as zero placeholders:
pname, plist (NIL), home-package name string.  BUILD-INITIAL-PACKAGES's
IMach branch FIXUP-SYMBOL-PACKAGEs T and NIL by hand (package.lisp:2412)
before the SYMBOL-AREA sweep: SYMBOL-PACKAGE must yield the home name
string -- the dist homes both in LISP -- and FIND-SYMBOL-LOCAL GET-PNAMEs
them (M3h boot 16).  Needs the heap regions (PNAME-AREA), so it cannot
run in MAKE-SKELETON-WORLD."
  (let ((dtp-header-p (cold-dtp w "HEADER-P"))
        (dtp-string (cold-dtp w "STRING"))
        (lisp-name (cold-pname w "LISP")))
    (loop for (vma pname) in (list (list (cold-world-nil-vma w) "NIL")
                                   (list (cold-world-t-vma w) "T"))
          do (cw-set w vma (tag 0 dtp-header-p) (cold-pname w pname))
             (multiple-value-bind (ntag ndata) (cold-nil-q w)
               (cw-set w (+ vma 3) ntag ndata))
             (cw-set w (+ vma 4) (tag 0 dtp-string) lisp-name))))

(defun cold-build-nil-t (w)
  "NIL and T at their architectural addresses (the emulator hard-sets its
NIL/T registers to these).  The wired region's allocation pointer resumes
right after T's block -- the original generator's first wired allocation
is *REGION-FREE-POINTER*'s header at NIL+#xD."
  (let ((nil-vma (cold-address w "NIL-ADDRESS"))
        (t-vma (cold-address w "T-ADDRESS"))
        (region (cold-area-current-region w "WIRED-CONTROL-TABLES")))
    (setf (cold-world-nil-vma w) nil-vma
          (cold-world-t-vma w) t-vma)
    (cold-build-symbol-block w nil-vma
                             :value-tag (tag 0 (cold-dtp w "NIL"))
                             :value-data nil-vma)
    (cold-build-symbol-block w t-vma
                             :value-tag (tag 0 (cold-dtp w "SYMBOL"))
                             :value-data t-vma)
    ;; The 3-Q gap between NIL's block and T holds the distribution's
    ;; "Benson" string: MAP-OVER-OBJECTS-IN-REGION starts its wired walk
    ;; at NIL and %FIND-STRUCTURE-EXTENT must parse every Q up to the
    ;; free pointer -- unwritten NULLs here backscan to NIL's header and
    ;; die "Found non-enclosing structure" (M3h boot 24).
    (cw-set w (+ nil-vma 5)
            (tag (layout-value (cold-world-layout w)
                               "SYSTEM:%HEADER-TYPE-ARRAY")
                 (cold-dtp w "HEADER-I"))
            #x50000006)
    (cw-set w (+ nil-vma 6) (tag 0 (cold-dtp w "FIXNUM")) #x736E6542) ; "Bens"
    (cw-set w (+ nil-vma 7) (tag 0 (cold-dtp w "FIXNUM")) #x00006E6F) ; "on"
    ;; The trap page and comm pages below NIL are stored with cw-set
    ;; directly; allocation starts after T.
    (setf (cold-region-free region) (+ t-vma 5)))
  w)

(defun cold-reserve-wired-array (w area type-name length &key leader-length
                                                              named-structure)
  "Allocate an array shell (optional leader + header) in AREA and return
the header vma.  Data Qs are left for the machinery fill pass.  Mirrors
cold-array's layout: [leader-header][leader elts reversed][header][data]."
  (let* ((code (cold-array-type-code w type-name))
         (packing (ldb (byte 3 1) code))
         (nwords (if (zerop packing) length (ceiling length (ash 1 packing))))
         (leader-length (or leader-length 0))
         (total (+ (if (zerop leader-length) 0 (1+ leader-length)) 1 nwords))
         (base (cold-alloc w area total))
         (header (if (zerop leader-length) base (+ base 1 leader-length))))
    (unless (zerop leader-length)
      (cw-set w base
              (tag (layout-value (cold-world-layout w)
                                 "SYSTEM:%HEADER-TYPE-LEADER")
                   (cold-dtp w "HEADER-P"))
              header)
      ;; Leader elements start NIL; the fill pass sets the fill pointer.
      (multiple-value-bind (ntag ndata) (cold-nil-q w)
        (dotimes (i leader-length)
          (cw-set w (- header 1 i) ntag ndata))))
    (cw-set w header (tag 1 (cold-dtp w "HEADER-I"))
            (logior (ash code 26)
                    (if named-structure (ash 1 25) 0)
                    (ash leader-length 15)
                    length))
    header))

(defun cold-reserve-storage-tables (w)
  "Reserve the wired region tables (first allocations after T, region 1)
and the SAFEGUARDED area/region/oblast tables (first allocations in region
2) at their ground-truth addresses.  Header vmas land in the machinery
plist; contents are written by COLD-FILL-STORAGE-TABLES after the load."
  (flet ((reserve (key area type len &key leader)
           (setf (getf (cold-world-machinery w) key)
                 (cold-reserve-wired-array w area type len
                                           :leader-length leader))))
    ;; Wired region tables, in the distribution's allocation order.
    (reserve :region-free-pointer  "WIRED-CONTROL-TABLES" "ART-Q" 1024)
    (reserve :region-gc-pointer    "WIRED-CONTROL-TABLES" "ART-Q" 1024)
    (reserve :region-quantum-origin "WIRED-CONTROL-TABLES" "ART-Q" 1024)
    (reserve :region-quantum-length "WIRED-CONTROL-TABLES" "ART-Q" 1024)
    (reserve :region-bits          "WIRED-CONTROL-TABLES" "ART-Q" 1024)
    ;; Safeguarded storage tables.  The five area tables carry 2-Q leaders
    ;; (fill pointer = number of areas); the region tables don't.
    (reserve :area-name            "SAFEGUARDED-OBJECTS-AREA" "ART-Q-LIST" 128
             :leader 1)
    (reserve :area-maximum-quantum-size "SAFEGUARDED-OBJECTS-AREA"
             "ART-Q-LIST" 128 :leader 1)
    (reserve :area-region-quantum-size "SAFEGUARDED-OBJECTS-AREA"
             "ART-Q-LIST" 128 :leader 1)
    (reserve :area-region-list     "SAFEGUARDED-OBJECTS-AREA" "ART-16B" 128
             :leader 1)
    (reserve :area-region-bits     "SAFEGUARDED-OBJECTS-AREA" "ART-Q-LIST" 128
             :leader 1)
    (reserve :region-free-pointer-before-flip "SAFEGUARDED-OBJECTS-AREA"
             "ART-Q" 1024)
    (reserve :region-list-thread   "SAFEGUARDED-OBJECTS-AREA" "ART-16B" 1024)
    (reserve :region-created-pages "SAFEGUARDED-OBJECTS-AREA" "ART-Q" 1024)
    (reserve :region-area          "SAFEGUARDED-OBJECTS-AREA" "ART-16B" 1024)
    (reserve :oblast-free-size     "SAFEGUARDED-OBJECTS-AREA" "ART-8B" 2048)))

(defun cold-build-catch-all (w)
  "A minimal CCA whose two instruction Qs are packed (halt, halt) then
end-of-code.  Every trap vector points at its entry until the cold set
installs real handlers, so any stray trap halts the machine with a PC
that names the vector.  Wrapped as a well-formed compiled function:
ITRAP-DISPATCH's entry-T sweep retires it during the load, but the dead
block stays in the wired region and BOOTSTRAP-FORWARD-SYMBOL-CELLS'
MAP-COMPILED-FUNCTIONS walk must still %FIND-STRUCTURE-EXTENT past it
(M3h boot 24) and CCA-EXTRA-INFO must read inside it: pass 1 takes
COMPILED-FUNCTION-NAME = (CAR extra-info) where extra-info is the Q at
cca + (total-size - suffix-size), so suffix-size 0 reads one Q PAST the
block (M3h boot 25).  Suffix 1 with a NIL extra-info gives name = NIL,
which FDEFINEDP rejects, and pass 1 skips the block."
  (let ((cca (cold-alloc w "WIRED-CONTROL-TABLES" 4)))
    ;; CCA header: suffix-size 1, total-size 4 (sys2/macro.lisp
    ;; CCA-EXTRA-INFO #+IMACH: CCA-SUFFIX-SIZE = (byte * 18)).
    (cw-set w cca
            (tag (layout-value (cold-world-layout w)
                               "SYSTEM:%HEADER-TYPE-COMPILED-FUNCTION")
                 (cold-dtp w "HEADER-I"))
            (logior (ash 1 18) 4))
    (cw-set w (+ cca 1) (tag 0 (cold-dtp w "COMPILED-FUNCTION")) (+ cca 2))
    ;; Packed-instruction-62 carries two 18-bit halfwords: (halt 0, halt 0).
    (cw-set w (+ cca 2) (tag 0 (cold-dtp w "PACKED-INSTRUCTION-62"))
            #xF000BC00)
    ;; Suffix: the extra-info list, NIL (real CCAs end code the same way,
    ;; with a cdr-1 suffix Q directly after the last instruction).
    (cw-set w (+ cca 3) (tag 1 (cold-dtp w "NIL")) (cold-world-nil-vma w))
    (setf (cold-world-catch-all-pc w) (+ cca 2))))

(defun cold-build-trap-vectors (w)
  "Fill all %TRAP-VECTOR-LENGTH slots with the catch-all, trap mode 0.
SET-TRAP-VECTOR-ENTRY overwrites the defined ones during the cold load."
  (let ((base (cold-address w "%TRAP-VECTOR-BASE"))
        (length (layout-value (cold-world-layout w) "%TRAP-VECTOR-LENGTH"))
        (even-pc (cold-dtp w "EVEN-PC"))
        (pc (cold-world-catch-all-pc w)))
    (dotimes (i length)
      (cw-set w (+ base i) (tag 0 even-pc) pc))))

(defun cold-touch-comm-areas (w)
  "FEP and SYSCOM communication pages: present but zero.  Slot values are
installed by DEFINE-MAGIC-LOCATIONS-1 handling during the cold load."
  (dolist (block (layout-section (cold-world-layout w) :magic-locations))
    (destructuring-bind (name start end ventries) block
      (declare (ignore name ventries))
      ;; BOOT-COMM lives at physical #o777400000 outside the VMA=PMA window;
      ;; the emulator's life-support owns it.  Only architectural blocks
      ;; inside the wired zone get pages in the world file.
      (when (and (>= start #xF8000000) (< end +wired-zone-limit+))
        (loop for vma from (logandc2 start #xFF) below end
              by +ivory-page-size-qs+
              do (cw-touch w vma))))))

(defun make-skeleton-world (layout)
  "M3a entry point: the wired furniture without any loaded objects."
  (let ((w (make-cold-world :layout (if (layout-p layout)
                                        layout
                                        (read-layout layout)))))
    (cold-init-areas w)
    (cold-init-architectural-regions w)
    (cold-build-nil-t w)
    (cold-reserve-storage-tables w)
    (cold-build-catch-all w)
    (cold-build-trap-vectors w)
    (cold-touch-comm-areas w)
    w))
