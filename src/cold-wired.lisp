;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: the architecturally-fixed wired furniture.
;;;
;;; M3a scope: trap-vector page (every slot filled with a catch-all even-pc
;;; -- the emulator halts with IllegalTrapVector on anything else), zeroed
;;; FEP/SYSCOM communication pages, and the hand-built NIL/T page.  The
;;; original generator's first wired allocation starts immediately after T
;;; (ground truth: *REGION-FREE-POINTER*'s header at NIL+#xD), so the wired
;;; region here does the same.
;;;
;;; Later stages replace the placeholder cells: NIL/T value cells become
;;; one-q-forwards into the wired symbol-cell tables (M3e), pname/plist/
;;; package cells point at real objects once materializers exist (M3b).

(in-package #:worldtool)

;;; End of the wired VMA=PMA zone we may allocate into.  Ground truth ends
;;; at #xF804A600 (SYSCOM %WIRED-VIRTUAL-ADDRESS-HIGH); staying inside it
;;; keeps the fresh world's wired footprint comparable.
(defconstant +wired-zone-limit+ #xF804A600)

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

(defun cold-build-nil-t (w)
  "NIL and T at their architectural addresses (the emulator hard-sets its
NIL/T registers to these), plus the wired region whose free pointer starts
right after T."
  (let ((nil-vma (cold-address w "NIL-ADDRESS"))
        (t-vma (cold-address w "T-ADDRESS")))
    (setf (cold-world-nil-vma w) nil-vma
          (cold-world-t-vma w) t-vma)
    ;; Wired allocations live in the same region NIL/T open.
    (let ((region (cold-add-region w "WIRED-CONTROL-TABLES"
                                   nil-vma (- +wired-zone-limit+ nil-vma))))
      (cold-build-symbol-block w nil-vma
                               :value-tag (tag 0 (cold-dtp w "NIL"))
                               :value-data nil-vma)
      (cold-build-symbol-block w t-vma
                               :value-tag (tag 0 (cold-dtp w "SYMBOL"))
                               :value-data t-vma)
      ;; NIL+5..T-1 stays zero (ground truth keeps unrelated heap objects
      ;; there); allocation resumes after T's block.
      (setf (cold-region-free region) (+ t-vma 5))))
  w)

(defun cold-build-catch-all (w)
  "A two-Q instruction block: packed (halt, halt) then end-of-code.  Every
trap vector points here until the cold set installs real handlers, so any
stray trap halts the machine with a PC that names the vector."
  (let ((vma (cold-alloc w "WIRED-CONTROL-TABLES" 2)))
    ;; Packed-instruction-62 carries two 18-bit halfwords: (halt 0, halt 0).
    (cw-set w vma (tag 0 (cold-dtp w "PACKED-INSTRUCTION-62")) #xF000BC00)
    ;; End of compiled code: cdr 1, dtp-null, 0 (stub/idispat.c DoICacheFill).
    (cw-set w (+ vma 1) (tag 1 (cold-dtp w "NULL")) 0)
    (setf (cold-world-catch-all-pc w) vma)))

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
installed by DEFINE-MAGIC-LOCATIONS-1 handling (M3e)."
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
    (cold-build-nil-t w)
    (cold-build-catch-all w)
    (cold-build-trap-vectors w)
    (cold-touch-comm-areas w)
    w))
