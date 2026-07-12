;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: the wired machinery (M3e).
;;;
;;; Four generator responsibilities the cold set itself can't perform:
;;;
;;; 1. DEFINE-MAGIC-LOCATIONS-1 (i-sys/sysdf1.lisp:55): forward each
;;;    variable's value cell -- or (FUNCTION f)'s function cell -- into its
;;;    communication-block slot, moving the current cell contents there.
;;;    Runs at sysdf1 load time (cold-eval dispatch), so later SETQs and
;;;    FDEFINEs write through, and DSCL :WIRED sees the cell already
;;;    forwarded.  BOOT-COMM slots (#xFFFE0000, host-owned) are forwarded
;;;    to but never written.
;;;
;;; 2. The region/area/oblast storage tables reserved by cold-wired at the
;;;    distribution addresses, filled from this world's areas and regions.
;;;    Allocation reads them from instruction one (i-allocate's escape
;;;    handlers AREF the arrays behind the SYSCOM slot variables), so they
;;;    must describe the cold world exactly; INITIALIZE-STORAGE rebuilds
;;;    the dynamic parts (address-space map, PHT windows) at first boot.
;;;
;;; 3. The initial stack group: MAKE-STACK-GROUP's durable fields
;;;    (istack.lisp:1020-1084) on a wired 64-Q named-structure array plus
;;;    the control/binding stack allocations.  Data-stack fields stay NIL:
;;;    GROW-DATA-STACK allocates the initial data stack at first boot
;;;    (allocate-common.lisp:990).
;;;
;;; 4. Verbatim IFEP trap vectors: mode-3 FEP-mode handlers point into the
;;;    IFEP kernel (#xF801xxxx, loaded at fixed addresses from the
;;;    VLM_debugger world) and the generic/message-dispatch + fence slots
;;;    are constant Qs whose targets are unmapped even in the distribution
;;;    world.  Copied from the reference world, which is already a required
;;;    coldgen input (symbol-home oracle).
;;;
;;; 5. Verbatim IFEP FEPComm function slots (M3g): the 19 debugger-kernel
;;;    entry points FEP-COMMAND-STRING .. FEP-SEQUENCE-BREAK (FEPComm
;;;    slots #x1F-#x31).  No cold file defines them; the slots are
;;;    magic-forwarded function cells, so copying the reference Q
;;;    (1C:F801xxxx) IS the fdefine.

(in-package #:worldtool)

;;; ---------------- Ref-derived area configuration ----------------

;;; Areas 0-21 are the cold set (creation order = distribution numbering;
;;; areas 22+ are made warm by QLD).  BITS is the distribution's
;;; *AREA-REGION-BITS* template ("initial value for REGION-BITS"), RQSZ its
;;; *AREA-REGION-QUANTUM-SIZE*.  Read from Genera-8-5.vlod (probe 2026-07-04).
(defparameter *cold-area-config*
  ;; number name-suffix                bits        rqsz
  '((0  "FEP-AREA"                  #x00800008 #x20)
    (1  "WIRED-CONTROL-TABLES"      #x00800048 #x20)
    (2  "SAFEGUARDED-OBJECTS-AREA"  #x02840048 #x20)
    (3  "CONTROL-STACK-AREA"        #x02840068 #x20)
    (4  "STRUCTURE-STACK-AREA"      #x02840064 #x20)
    (5  "BINDING-STACK-AREA"        #x0284006C #x20)
    (6  "STACK-AREA"                #x02840060 #x20)
    (7  "CONSTANTS-AREA"            #x028800C8 #x20)
    (8  "WORKING-STORAGE-AREA"      #x02040049 #x20)
    (9  "PERMANENT-STORAGE-AREA"    #x02880048 #x20)
    (10 "PROPERTY-LIST-AREA"        #x028C0048 #x20)
    (11 "PNAME-AREA"                #x02880048 #x20)
    (12 "SYMBOL-AREA"               #x02880048 #x20)
    (13 "COMPILED-FUNCTION-AREA"    #x02880048 #x20)
    (14 "DEBUG-INFO-AREA"           #x02880048 #x20)
    (15 "WIRED-DYNAMIC-AREA"        #x01840048 #x20)
    (16 "PAGE-TABLE-AREA"           #x01840008 #x20)
    (17 "GC-TABLE-AREA"             #x01840008 #x20)
    (18 "PKG-AREA"                  #x02880049 #x20)
    (19 "*WIRED-CONSOLE-AREA*"      #x02840049 #x20)
    (20 "DISK-ARRAY-AREA"           #x02840049 #x04)
    (21 "SOURCE-LOCATOR-AREA"       #x028C0049 #x20)))

;;; +cold-area-count+ (= 22, the areas above) lives in cold-model.lisp:
;;; the mini-eval's MAKE-AREA arm needs it and cold-eval loads first.

(defparameter *cold-synthesized-boot-areas* '(24)
  "Boot areas registered from the layout + reference rows because their
creating MAKE-AREA files are not in the cold set.  24
BIG-PACKET-CONS-AREA has NO creator anywhere in the rel-8-5 sources (a
pre-8.5 vintage survivor); it stays synthesized so the ARRAY-PUSH
numbering of everything after it holds.  22/23 were synthesized until
SYS: NETWORK; PKTS rejoined the cold set (its .vbin recompiled
2026-07-10); unregistered numbers shift every later area and (N-AREAS)
rejects them at first cons (M3h boot 31).")

(defun cold-live-boot-areas (w)
  "Sorted (area-number . name-vsym) of every area beyond the generator's
own that must be registered in the area tables: the cold set's MAKE-AREA
records plus the synthesized allowlist."
  (sort (append
         (loop for n in *cold-synthesized-boot-areas*
               unless (assoc n (cold-world-boot-areas w))
                 collect (let* ((full (cold-area-name (cold-area w n)))
                                (colon (position #\: full)))
                           (cons n (make-vsym (subseq full 0 colon)
                                              (subseq full (1+ colon))))))
         (copy-list (cold-world-boot-areas w)))
        #'< :key #'car))

;;; Distribution REGION-BITS for the architectural regions 0-5 (probe).
(defparameter *cold-architectural-region-bits*
  #(#x00800008 #x00800049 #x02840049 #x02840068 #x0284006C #x02840060))

;;; %%REGION- fields (layout DEFSYSBYTES): representation bits 0-1,
;;; space-type bits 2-5, scavenge-enable bit 6, level bits 18-23.
(defconstant +region-bits-scavenge+ (ash 1 6))

(defun cold-heap-region-bits (area-bits rep)
  "REGION-BITS for a generator heap region: the area template with the
representation set from the region's REP (STRUCTURE=1, LIST=0 -- the
dist's PERMANENT-STORAGE-AREA carries 02880048 LIST beside 02880049
STRUCTURE regions, both the 02880048 template) and an ephemeral-level
template (level < 32, e.g. WORKING-STORAGE-AREA's level-1 marker)
replaced by plain dynamic level 35 -- the cold world boots with the
ephemeral GC off."
  (let ((bits (dpb (ecase rep (:structure 1) (:list 0)) (byte 2 0)
                   area-bits)))
    (if (< (ldb (byte 6 18) bits) 32)
        (dpb 35 (byte 6 18) bits)
        bits)))

;;; MAKE-STACK-GROUP parameters (istack.lisp:1020-1084).
(defconstant +cold-control-stack-size+ #o30000)   ; regular-pdl-size
(defconstant +cold-binding-stack-size+ #o4000)    ; special-pdl-size
;; SG-STATUS-BITS: uninitialized (bit 6) + safe (bit 9).
(defconstant +cold-sg-status+ #x240)

;;; ---------------- DEFINE-MAGIC-LOCATIONS-1 ----------------

(defun cold-magic-block-vma (options)
  "Base VMA of a magic block from its options plist (vsym keywords).
Only :PHYSICAL-ADDRESS appears in the cold set; vma=pma maps it at
#xF8000000 + physical (BOOT-COMM wraps to #xFFFE0000)."
  (loop for (key value) on options by #'cddr
        when (and (vsym-p key) (string= (vsym-name key) "PHYSICAL-ADDRESS"))
          return (ldb (byte 32 0) (+ #xF8000000 value))
        finally (error "Magic block options ~S: no PHYSICAL-ADDRESS" options)))

(defun cold-magic-var-cell (w var)
  "(values cell-vma kind) for one DEFINE-MAGIC-LOCATIONS variable entry:
a symbol forwards its value cell, (FUNCTION f) its function cell."
  (cond ((vsym-p var)
         (values (+ (cold-vsym w var) 1) :value))
        ((and (consp var) (vsym-p (first var))
              (string= (vsym-name (first var)) "FUNCTION")
              (vsym-p (second var)))
         (values (+ (cold-vsym w (second var)) 2) :function))
        (t (error "Unsupported magic-location entry ~S" var))))

(defun cold-do-define-magic-locations (w parsed)
  "PARSED = (block-name options var-list), already unquoted.  Forward each
variable's cell to its consecutive slot, moving the current cell contents
into the slot (matching the distribution: an unbound cell moves as
NULL:symbol).  Slots outside the world file (BOOT-COMM) get the forward
but no store."
  (destructuring-bind (block-name options vars) parsed
    (declare (ignore block-name))
    (let* ((base (cold-magic-block-vma options))
           (in-world (< base +wired-zone-limit+))
           (fwd (cold-dtp w "ONE-Q-FORWARD")))
      (loop for var in vars
            for slot from base
            do (multiple-value-bind (cell kind) (cold-magic-var-cell w var)
                 (declare (ignore kind))
                 (let ((final (cold-follow-cell w cell)))
                   (when in-world
                     (multiple-value-bind (tag data) (cw-ref w final)
                       (cw-set w slot (tag 0 (tag-type tag)) data)))
                   ;; Forward the symbol's own cell, preserving cdr bits.
                   (multiple-value-bind (tag data) (cw-ref w cell)
                     (declare (ignore data))
                     (cw-set w cell (logior (logand tag #xC0) fwd) slot))))))))

;;; ---------------- Packed-array element access ----------------

(defun cold-table-set (w header index tag data)
  (cw-set w (+ header 1 index) tag data))

(defun cold-table-set16 (w header index value)
  "Write a 16-bit element of an ART-16B table (2 per word, low half first)."
  (let* ((word-vma (+ header 1 (floor index 2)))
         (fixnum (cold-dtp w "FIXNUM")))
    (multiple-value-bind (tag old) (cw-ref w word-vma)
      (declare (ignore tag))
      (cw-set w word-vma (tag 0 fixnum)
              (dpb value (byte 16 (* 16 (mod index 2))) old)))))

(defun cold-table-set8 (w header index value)
  (let* ((word-vma (+ header 1 (floor index 4)))
         (fixnum (cold-dtp w "FIXNUM")))
    (multiple-value-bind (tag old) (cw-ref w word-vma)
      (declare (ignore tag))
      (cw-set w word-vma (tag 0 fixnum)
              (dpb value (byte 8 (* 8 (mod index 4))) old)))))

(defun cold-table-ref16 (w header index)
  (multiple-value-bind (tag data) (cw-ref w (+ header 1 (floor index 2)))
    (declare (ignore tag))
    (ldb (byte 16 (* 16 (mod index 2))) data)))

(defun cold-set-fill-pointer (w header count)
  "Leader element 0 (at header-1) is the fill pointer."
  (cw-set w (- header 1) (tag 0 (cold-dtp w "FIXNUM")) count))

;;; ---------------- Storage tables ----------------

(defun cold-machinery (w key)
  (or (getf (cold-world-machinery w) key)
      (error "Machinery table ~S was never reserved" key)))

;;; The SYSCOM variables holding the storage-table arrays.
(defparameter *cold-table-variables*
  '((:area-name                       "*AREA-NAME*")
    (:area-maximum-quantum-size       "*AREA-MAXIMUM-QUANTUM-SIZE*")
    (:area-region-quantum-size        "*AREA-REGION-QUANTUM-SIZE*")
    (:area-region-list                "*AREA-REGION-LIST*")
    (:area-region-bits                "*AREA-REGION-BITS*")
    (:region-quantum-origin           "*REGION-QUANTUM-ORIGIN*")
    (:region-quantum-length           "*REGION-QUANTUM-LENGTH*")
    (:region-free-pointer             "*REGION-FREE-POINTER*")
    (:region-gc-pointer               "*REGION-GC-POINTER*")
    (:region-bits                     "*REGION-BITS*")
    (:region-list-thread              "*REGION-LIST-THREAD*")
    (:region-area                     "*REGION-AREA*")
    (:region-created-pages            "*REGION-CREATED-PAGES*")
    (:region-free-pointer-before-flip "*REGION-FREE-POINTER-BEFORE-FLIP*")
    (:oblast-free-size                "*OBLAST-FREE-SIZE*")))

(defparameter *cold-boot-area-region-bits*
  '((26 . #x02880048))
  "Distribution *AREA-REGION-BITS* templates for boot-created areas
(>= +cold-area-count+) that receive generator objects, so their
build-time regions get bits like any config area's.  26
FLAVOR:*FLAVOR-STATIC-AREA* (\":GC :STATIC :REPRESENTATION :LIST\",
flavor/global.lisp:113) hosts the forged MAKE-INSTANCE generic-function
object (M3h boot 36); template probed from the dist's area row 26 at
*AREA-REGION-BITS*+1+26 (its region 82 carries the same bits at
level 34).")

(defun cold-region-bits-for (w region)
  (let ((n (cold-region-number region))
        (area (cold-region-area region)))
    (cond ((< n (length *cold-architectural-region-bits*))
           (aref *cold-architectural-region-bits* n))
          (t
           (let* ((config (assoc area *cold-area-config*))
                  (boot (assoc area *cold-boot-area-region-bits*))
                  (bits (cond
                          ((and config (member area '(16 17)))
                           ;; PAGE-TABLE / GC-TABLE
                           (third config))
                          (config
                           (cold-heap-region-bits (third config)
                                                  (cold-region-rep region)))
                          (boot
                           (cold-heap-region-bits (cdr boot)
                                                  (cold-region-rep region)))
                          (t
                           (error "Region ~D belongs to non-cold area ~D"
                                  n area)))))
             ;; The saved heap (zone 16) is uniformly STATIC level 36,
             ;; exactly like the distribution's zone-16 regions: one
             ;; level per zone is an architectural invariant
             ;; (UPDATE-ZONE-AND-DEMILEVEL-TABLES errors on mismatch),
             ;; and the area templates' 34/35 only shape regions the
             ;; runtime creates later in fresh zones.
             (if (= (ldb (byte 5 27) (cold-region-origin region)) 16)
                 (dpb 36 (byte 6 18) bits)
                 bits))))))

(defun cold-region-has-pages-p (w region)
  "Does any world page fall inside the region?  (FEP-AREA and reserved
address space like PAGE-TABLE-AREA have none.)"
  (loop for vma from (cold-region-origin region)
          below (cold-region-free region) by +ivory-page-size-qs+
        thereis (nth-value 2 (cw-ref w vma))))

(defun cold-fill-storage-tables (w &key reference)
  "Write the wired region tables and safeguarded area/oblast tables from
this world's areas and regions.  REFERENCE supplies the qsize/maxq/bits
rows of the areas the cold set itself creates via MAKE-AREA (boot 31)."
  (let ((fixnum (cold-dtp w "FIXNUM"))
        (regions (cold-world-regions w))
        (layout (cold-world-layout w)))
    (multiple-value-bind (ntag ndata) (cold-nil-q w)
      ;; --- Area tables (128 entries, fill pointer = cold area count).
      (let ((name-tbl (cold-machinery w :area-name))
            (maxq-tbl (cold-machinery w :area-maximum-quantum-size))
            (rqsz-tbl (cold-machinery w :area-region-quantum-size))
            (rlist-tbl (cold-machinery w :area-region-list))
            (abits-tbl (cold-machinery w :area-region-bits)))
        (dotimes (a 128)
          (let ((config (assoc a *cold-area-config*)))
            (cold-table-set w maxq-tbl a (tag 0 fixnum) #x10000)
            (cold-table-set w rqsz-tbl a (tag 0 fixnum)
                            (if config (fourth config) 0))
            (cold-table-set w abits-tbl a (tag 0 fixnum)
                            (if config (third config) 0))
            ;; Any area owning build-time regions -- config areas AND
            ;; boot areas like 26 *FLAVOR-STATIC-AREA*, which hosts the
            ;; forged MAKE-INSTANCE generic (M3h boot 36) -- threads
            ;; them from its REGION-LIST row.
            (cold-table-set16 w rlist-tbl a
                              (let ((area (and (< a (length (cold-world-areas w)))
                                               (aref (cold-world-areas w) a))))
                                (if (and area (cold-area-regions area))
                                    (first (cold-area-regions area))
                                    #xFFFF)))
            (if config
                (let* ((full-name (cold-area-name (cold-area w a)))
                       (colon (position #\: full-name)))
                  (multiple-value-bind (st sd)
                      (cold-symbol-ref w (make-vsym (subseq full-name 0 colon)
                                                    (subseq full-name (1+ colon))))
                    (cold-table-set w name-tbl a st sd)))
                (cold-table-set w name-tbl a ntag ndata))))
        ;; --- Areas the cold set creates at load time (MAKE-AREA in a
        ;; cold file; the mini-eval assigned their layout numbers and
        ;; recorded them on the world).  Register the rows MAKE-AREA's
        ;; ARRAY-PUSHes would have written (allocate-common.lisp:647):
        ;; without them (N-AREAS) -- the *AREA-NAME* fill pointer --
        ;; rejects the area at its first cons, and the %ALLOCATE-*-BLOCK
        ;; escape handler's FERROR conses into the same area, recursing
        ;; to a stack overflow pre-banner (M3h boot 31,
        ;; FLAVOR:*FLAVOR-AREA* = 25 under DEFFLAVOR-INTERNAL).  The
        ;; qsize/maxq/bits rows are the deterministic output of
        ;; MAKE-AREA's keyword parse; rather than replicate that
        ;; ~200-line parse we copy the reference world's rows (its
        ;; tables live at the same vmas; rows 22..66 all carry
        ;; static-band levels 33-36, so no warm ephemeral-level state
        ;; leaks in).  REGION-LIST stays -1 for a boot-created area with
        ;; no build-time region (the boot's first cons allocates one);
        ;; *FLAVOR-STATIC-AREA*'s forged-generic region is the exception
        ;; and was already threaded by the 128-row loop above (M3h
        ;; boot 36).
        (let ((recorded (cold-live-boot-areas w)))
          (when (and recorded (not reference))
            (error "~D boot-created area~:P but no reference world"
                   (length recorded)))
          (loop for (n . name) in recorded
                for expect from +cold-area-count+
                do (unless (= n expect)
                     ;; ARRAY-PUSH numbering was contiguous in the
                     ;; original cold load; a hole means our cold set
                     ;; diverges from the one that built the dist.
                     (error "Boot area ~D (~A) breaks contiguity at ~D"
                            n (vsym-name name) expect))
                   (multiple-value-bind (st sd) (cold-symbol-ref w name)
                     (cold-table-set w name-tbl n st sd))
                   (dolist (tbl (list maxq-tbl rqsz-tbl abits-tbl))
                     (multiple-value-bind (tag data)
                         (world-q reference (+ tbl 1 n))
                       (unless (and tag (= (tag-type tag) fixnum))
                         (error "Reference area row ~D of table ~
#x~8,'0X is ~2,'0X:~8,'0X" n tbl (or tag 0) (or data 0)))
                       (cold-table-set w tbl n (tag 0 fixnum) data))))
          (let ((live (+ +cold-area-count+ (length recorded))))
            (dolist (tbl (list name-tbl maxq-tbl rqsz-tbl rlist-tbl
                               abits-tbl))
              (cold-set-fill-pointer w tbl live))
            ;; The name table doubles as the generator-owned
            ;; SI:AREA-LIST (ldata.lisp:201; M3h boot-8 trap): it is
            ;; ART-Q-LIST, cdr-next through the live areas with cdr-nil
            ;; on the last (dist element 66 tag 58), and AREA-LIST
            ;; references its data verbatim.
            (multiple-value-bind (tag data)
                (cw-ref w (+ name-tbl live))
              (cw-set w (+ name-tbl live) (logior #x40 tag) data))
            (cold-set-symbol-value w (make-vsym "SYSTEM-INTERNALS"
                                                "AREA-LIST")
                                   (tag 0 (cold-dtp w "LIST"))
                                   (+ name-tbl 1)))))
      ;; --- Region tables (1024 entries).
      (let ((fp-tbl (cold-machinery w :region-free-pointer))
            (gcp-tbl (cold-machinery w :region-gc-pointer))
            (org-tbl (cold-machinery w :region-quantum-origin))
            (len-tbl (cold-machinery w :region-quantum-length))
            (bits-tbl (cold-machinery w :region-bits))
            (fpbf-tbl (cold-machinery w :region-free-pointer-before-flip))
            (thread-tbl (cold-machinery w :region-list-thread))
            (created-tbl (cold-machinery w :region-created-pages))
            (area-tbl (cold-machinery w :region-area))
            (page-size (layout-value layout "PAGE-SIZE")))
        (dotimes (r 1024)
          (if (< r (fill-pointer regions))
              (let* ((region (aref regions r))
                     (bits (cold-region-bits-for w region))
                     (fp (- (cold-region-free region)
                            (cold-region-origin region)))
                     (created (if (cold-region-has-pages-p w region)
                                  (* (ceiling fp page-size) page-size)
                                  0)))
                (cold-table-set w org-tbl r (tag 0 fixnum)
                                (ash (cold-region-origin region) -16))
                (cold-table-set w len-tbl r (tag 0 fixnum)
                                (ceiling (cold-region-length region) #x10000))
                (cold-table-set w fp-tbl r (tag 0 fixnum) fp)
                (cold-table-set w gcp-tbl r (tag 0 fixnum)
                                (if (logtest bits +region-bits-scavenge+) fp 0))
                (cold-table-set w bits-tbl r (tag 0 fixnum) bits)
                (cold-table-set w fpbf-tbl r (tag 0 fixnum) fp)
                (cold-table-set w created-tbl r (tag 0 fixnum) created)
                (cold-table-set16 w area-tbl r (cold-region-area region)))
              (progn
                (dolist (tbl (list org-tbl len-tbl fp-tbl gcp-tbl bits-tbl
                                   fpbf-tbl created-tbl))
                  (cold-table-set w tbl r (tag 0 fixnum) 0))
                (cold-table-set16 w area-tbl r #xFFFF)))
          (cold-table-set16 w thread-tbl r #xFFFF))
        ;; Thread chains: newest region -> next older -> ... -> -1.
        (loop for area across (cold-world-areas w)
              when area
                do (loop for (r next) on (cold-area-regions area)
                         when next
                           do (cold-table-set16 w thread-tbl r next))))
      ;; --- The variables (magic-forwarded into SYSCOM) hold the arrays.
      (let ((array (cold-dtp w "ARRAY")))
        (loop for (key name) in *cold-table-variables*
              do (cold-set-symbol-value w (make-vsym "SYSTEM" name)
                                        (tag 0 array)
                                        (cold-machinery w key))))
      ;; --- Oblast free sizes: max run of consecutive free quanta.
      (let ((oblast-tbl (cold-machinery w :oblast-free-size))
            (used (make-array 65536 :element-type 'bit :initial-element 0)))
        (loop for region across regions
              do (loop for q from (ash (cold-region-origin region) -16)
                       repeat (ceiling (cold-region-length region) #x10000)
                       do (setf (sbit used q) 1)))
        (dotimes (oblast 2048)
          (let ((max-run 0) (run 0))
            (loop for q from (* oblast 32) repeat 32
                  do (if (zerop (sbit used q))
                         (setf max-run (max max-run (incf run)))
                         (setf run 0)))
            (cold-table-set8 w oblast-tbl oblast max-run)))))))

;;; ---------------- Initial stack group ----------------

(defun cold-sg-field-word (layout field)
  "Whole-word index of a STACK-GROUP defstorage field."
  (loop for entry in (layout-section layout :defstorage-fields)
        when (and (string= (first entry) "SYSTEM:STACK-GROUP")
                  (string= (strip-package (second entry)) field))
          return (fifth entry)
        finally (error "STACK-GROUP field ~A not in layout" field)))

(defun cold-alloc-stack (w area-name size)
  "Take SIZE Qs from the tail of the area's architectural region and
make the pages present.  Returns the base vma.  Stacks pack one after
another in the single region (%MAKE-STACK's ALLOCATE-OR-EXTEND-MAGIC-
REGION never makes a second table region -- the dist has exactly one
region per stack area, the grower's stacks tailing the initial SG's),
so every stack must be page-aligned and page-granular."
  (let* ((region (cold-area-current-region w area-name))
         (base (cold-region-free region)))
    (unless (and (zerop (mod base +ivory-page-size-qs+))
                 (zerop (mod size +ivory-page-size-qs+)))
      (error "~A stack at #x~8,'0X size #x~X not page-granular"
             area-name base size))
    (unless (<= (+ (- base (cold-region-origin region)) size)
                (cold-region-length region))
      (error "~A region full: stack of #x~X Qs at #x~8,'0X" area-name
             size base))
    (setf (cold-region-free region) (+ base size))
    (loop for vma from base below (+ base size) by +ivory-page-size-qs+
          do (cw-touch w vma))
    base))

(defun cold-build-initial-stack-group (w)
  "The 64-Q wired STACK-GROUP named-structure array with MAKE-STACK-GROUP's
durable fields (istack.lisp:1020-1084); volatile state is recomputed at
boot by SG-INITIALIZE / STACK-GROUP-PRESET.  Also allocates the control
and binding stacks and stamps %INITIAL-STACK-GROUP."
  (let* ((layout (cold-world-layout w))
         (locative (cold-dtp w "LOCATIVE"))
         (fixnum (cold-dtp w "FIXNUM"))
         (cs-low (cold-alloc-stack w "CONTROL-STACK-AREA"
                                   +cold-control-stack-size+))
         (bs-low (cold-alloc-stack w "BINDING-STACK-AREA"
                                   +cold-binding-stack-size+))
         (sg (cold-reserve-wired-array w "WIRED-CONTROL-TABLES" "ART-Q" 63
                                       :named-structure t))
         (float-mode
           (multiple-value-bind (tag data boundp)
               (cold-symbol-value-q
                w (make-vsym "SYSTEM" "*DEFAULT-FLOAT-OPERATING-MODE*"))
             (if (and boundp (= (tag-type tag) fixnum))
                 data
                 #x6C000000))))
    (setf (getf (cold-world-machinery w) :initial-stack-group) sg)
    (flet ((set-field (name tag data)
             (cw-set w (+ sg (cold-sg-field-word layout name)) tag data)))
      ;; All slots start NIL (element 0 = named-structure symbol).
      (multiple-value-bind (ntag ndata) (cold-nil-q w)
        (loop for i from 1 to 63
              do (cw-set w (+ sg i) ntag ndata)))
      (multiple-value-bind (st sd)
          (cold-symbol-ref w (make-vsym "SYSTEM" "STACK-GROUP"))
        (cw-set w (+ sg 1) st sd))
      (set-field "SG-NAME" (tag 0 (cold-dtp w "STRING"))
                 (cold-string* w "Initial Stack Group"
                               "WIRED-CONTROL-TABLES"))
      (set-field "SG-STATUS-BITS" (tag 0 fixnum) +cold-sg-status+)
      (set-field "SG-STACK-POINTER" (tag 0 locative) cs-low)
      (set-field "SG-CONTROL-STACK-LOW" (tag 0 locative) cs-low)
      (set-field "SG-CONTROL-STACK-LIMIT" (tag 0 locative)
                 (+ cs-low +cold-control-stack-size+
                    (- (layout-value layout "CONTROL-STACK-OVERFLOW-MARGIN"))
                    (- (layout-value layout "CONTROL-STACK-MAX-FRAME-SIZE"))))
      ;; Binding pairs start on even addresses; the bottom pair is unused
      ;; and the pointer names the top word of the current pair.
      (set-field "SG-BINDING-STACK-POINTER" (tag 0 locative) (+ bs-low 1))
      (set-field "SG-BINDING-STACK-LOW" (tag 0 locative) bs-low)
      (set-field "SG-BINDING-STACK-LIMIT" (tag 0 locative)
                 (+ bs-low +cold-binding-stack-size+ -1))
      (set-field "SG-FLOAT-OPERATING-MODE" (tag 0 fixnum) float-mode)
      (set-field "SG-FLOAT-OPERATION-STATUS" (tag 0 fixnum) 0)
      (set-field "SG-STRUCTURE-STACK-POINTER-COUNT" (tag 0 fixnum) 0)
      (set-field "SG-ERROR-TRAP-LEVEL" (tag 0 fixnum) 0)
      (set-field "SG-WIRED-FRAME-DESCRIPTOR" (tag 0 fixnum) 0)
      ;; Global block registers reference guaranteed-nonexistent memory
      ;; (%BOUNDARY-ZONE in the zone field).
      (set-field "SG-BAR-2" (tag 0 locative) #x7FE00000)
      (set-field "SG-BAR-3" (tag 0 locative) #x7FE00000))
    (cold-set-symbol-value w (make-vsym "SYSTEM" "%INITIAL-STACK-GROUP")
                           (tag 0 (cold-dtp w "ARRAY")) sg)
    (cold-set-symbol-value w (make-vsym "SYSTEM"
                                        "%CURRENT-STACK-GROUP-STATUS-BITS")
                           (tag 0 fixnum) +cold-sg-status+)
    sg))

(defun cold-build-stack-grower (w)
  "DBG:STACK-GROWER = (MAKE-STACK-GROUP \"Stack grower\"),
debugger/debugger-support.lisp:1531 (defvar-safeguarded; the file is NOT
cold, so the object is a generator obligation like the initial stack
group).  The wired control-stack-overflow handler STACK-GROUP-CALLs it
(istack.lisp:1761); unbound, any pre-warm stack overflow reads the
unbound cell inside the trap handler and double-faults to SI:AUX-HALT
(M3h boot 31).  Field semantics = MAKE-STACK-GROUP (istack.lisp:1013):
a 63-Q named STACK-GROUP array in SAFEGUARDED-OBJECTS-AREA with fresh
control/binding stacks.  Sizes are the dist grower's (control #x3000,
binding #x800; its object at dist #xF0006400, stacks tailing the
initial SG's) -- the rel-8-5 source defaults (30000/4000) postdate the
dist build.  The SG ships UNPRESET (SG-UNINITIALIZED-BIT set), exactly
like the original cold world: the warm \"Stack grower preset\" init
presets it before any recoverable overflow can use it."
  (let* ((layout (cold-world-layout w))
         (locative (cold-dtp w "LOCATIVE"))
         (fixnum (cold-dtp w "FIXNUM"))
         (cs-size #x3000)
         (bs-size #x800)
         (cs-low (cold-alloc-stack w "CONTROL-STACK-AREA" cs-size))
         (bs-low (cold-alloc-stack w "BINDING-STACK-AREA" bs-size))
         (sg (cold-reserve-wired-array w "SAFEGUARDED-OBJECTS-AREA"
                                       "ART-Q" 63 :named-structure t))
         (float-mode
           (multiple-value-bind (tag data boundp)
               (cold-symbol-value-q
                w (make-vsym "SYSTEM" "*DEFAULT-FLOAT-OPERATING-MODE*"))
             (if (and boundp (= (tag-type tag) fixnum))
                 data
                 #x6C000000))))
    (setf (getf (cold-world-machinery w) :stack-grower) sg)
    (flet ((set-field (name tag data)
             (cw-set w (+ sg (cold-sg-field-word layout name)) tag data)))
      ;; All slots start NIL (element 0 = named-structure symbol).
      (multiple-value-bind (ntag ndata) (cold-nil-q w)
        (loop for i from 1 to 63
              do (cw-set w (+ sg i) ntag ndata)))
      (multiple-value-bind (st sd)
          (cold-symbol-ref w (make-vsym "SYSTEM" "STACK-GROUP"))
        (cw-set w (+ sg 1) st sd))
      (set-field "SG-NAME" (tag 0 (cold-dtp w "STRING"))
                 (cold-string* w "Stack grower" "SAFEGUARDED-OBJECTS-AREA"))
      (set-field "SG-STATUS-BITS" (tag 0 fixnum) +cold-sg-status+)
      (set-field "SG-STACK-POINTER" (tag 0 locative) cs-low)
      (set-field "SG-CONTROL-STACK-LOW" (tag 0 locative) cs-low)
      (set-field "SG-CONTROL-STACK-LIMIT" (tag 0 locative)
                 (+ cs-low cs-size
                    (- (layout-value layout "CONTROL-STACK-OVERFLOW-MARGIN"))
                    (- (layout-value layout "CONTROL-STACK-MAX-FRAME-SIZE"))))
      (set-field "SG-BINDING-STACK-POINTER" (tag 0 locative) (+ bs-low 1))
      (set-field "SG-BINDING-STACK-LOW" (tag 0 locative) bs-low)
      (set-field "SG-BINDING-STACK-LIMIT" (tag 0 locative)
                 (+ bs-low bs-size -1))
      (set-field "SG-FLOAT-OPERATING-MODE" (tag 0 fixnum) float-mode)
      (set-field "SG-FLOAT-OPERATION-STATUS" (tag 0 fixnum) 0)
      (set-field "SG-STRUCTURE-STACK-POINTER-COUNT" (tag 0 fixnum) 0)
      (set-field "SG-ERROR-TRAP-LEVEL" (tag 0 fixnum) 0)
      (set-field "SG-WIRED-FRAME-DESCRIPTOR" (tag 0 fixnum) 0)
      (set-field "SG-BAR-2" (tag 0 locative) #x7FE00000)
      (set-field "SG-BAR-3" (tag 0 locative) #x7FE00000))
    (cold-set-symbol-value w (make-vsym "DEBUGGER" "STACK-GROWER")
                           (tag 0 (cold-dtp w "ARRAY")) sg)
    sg))

(defun cold-build-make-instance-generic (w)
  "The MAKE-INSTANCE generic-function object, forged at build time
\(dist ground truth: DTP-GENERIC-FUNCTION at #x8800807C, area 26
*FLAVOR-STATIC-AREA*'s LIST region).  The cold set defers 7 MAKE-INSTANCE
method fdefines (flavor/vanilla.lisp:111, 5 in io/useful-streams.lisp,
sys2/hash.lisp's BASIC-HASH-TABLE -- all legitimately cold: sysdcl.lisp
inner cold subsystems, and useful-streams is in NO QLD mini-alist, so
withholding them loses the methods -- boot-26 dribbl precedent).  At
first boot each routes through
METHOD-FUNCTION-SPEC-HANDLER -> FIND-METHOD-HOLDER, which with
CREATE-P=T unconditionally FIND-GENERIC-FUNCTIONs the generic name with
CREATE (defmethod.lisp:727); no GF -> DEFGENERIC-INTERNAL ->
INSTALL-GENERIC-FUNCTION (defgeneric.lisp:833) finds MAKE-INSTANCE
already fdefined -- to the engineered bridge stub MAKE-INSTANCE-COLD
\(cold-load.lisp:142) -- and its conflict arm calls YES-OR-NO-P
\(defgeneric.lisp:861): pre-banner terminal I/O, *TERMINAL-IO* unbound,
trap (M3h boot 36).  With the GF pre-existing, FIND-GENERIC-FUNCTION
finds it via (GET 'MAKE-INSTANCE 'GENERIC) (defgeneric.lisp:340) and
FIND-METHOD-HOLDER's install arm is deflected by
GENERIC-FUNCTION-HAS-DISPATCH-FUNCTION (\"Generic function object
doesn't belong in function definition\"): holder filed, flavor PUSHNEWed
into the FLAVORS field, bridge fcell untouched.  At QLD, make.lisp:1058's
explicit DEFGENERIC updates this object's fields IN PLACE
\(defgeneric.lisp:60: GF objects are interned, modified rather than
replaced -- the dist object's ARGLIST/DEBUGGING-INFO point into the QLD
band, proving QLD updated the COLD object) and installs the real
dispatch (INSTALL's query arm skipped: EXPLICIT is set).
  Shape = the 7-Q SYSDEF DEFSTORAGE (field comments defgeneric.lisp:75):
NAME / ARGLIST / DEBUGGING-INFO / METHOD-COMBINATION / FLAGS / FLAVORS /
SELECTOR.  ARGLIST and DEBUGGING-INFO ship NIL (QLD overwrites; the dist
cold values are disposable), METHOD-COMBINATION = (:TWO-PASS) (dist
field -> 1-Q cdr-nil list of the keyword), FLAGS = #x81 = EXPLICIT +
HAS-DISPATCH-FUNCTION (dist #x281 adds COMPRESSED-DEBUGGING-INFO bit 9,
wrong for our NIL Q2; METHODS-MADE stays clear), FLAVORS = NIL (boot
methods file themselves in), SELECTOR = the object itself
\(DEFGENERIC-INTERNAL: non-message generics dispatch on the GF object,
dist 5D:8800807C self-pointer)."
  (let* ((dtp-symbol (cold-dtp w "SYMBOL"))
         (dtp-list (cold-dtp w "LIST"))
         (dtp-fixnum (cold-dtp w "FIXNUM"))
         (dtp-gf (cold-dtp w "GENERIC-FUNCTION"))
         ;; Resolve exactly as the deferred method fspecs' vsyms and the
         ;; cold defmethod/defgeneric CCA constants do -- under the
         ;; FLAVOR package context through the symbol-home oracle -- so
         ;; boot's (GET NAME 'GENERIC) read (NAME from the fspec,
         ;; GENERIC compiled into defgeneric.lisp) hits this property.
         (mi-sym (cold-vsym w (make-vsym "FLAVOR" "MAKE-INSTANCE")))
         (generic-sym (cold-vsym w (make-vsym "FLAVOR" "GENERIC")))
         (two-pass (cold-vsym w (make-vsym "KEYWORD" "TWO-PASS")))
         (gf (cold-alloc w "*FLAVOR-STATIC-AREA*" 7 :list))
         (mc (cold-alloc w "*FLAVOR-STATIC-AREA*" 1 :list)))
    (multiple-value-bind (ntag ndata) (cold-nil-q w)
      (flet ((set-q (offset cdr tag data)
               (cw-set w (+ gf offset)
                       (logior (ash cdr 6) (tag-type tag)) data)))
        (set-q 0 +cdr-next+ dtp-symbol mi-sym)           ; NAME
        (set-q 1 +cdr-next+ (tag-type ntag) ndata)       ; ARGLIST
        (set-q 2 +cdr-next+ (tag-type ntag) ndata)       ; DEBUGGING-INFO
        (set-q 3 +cdr-next+ dtp-list mc)                 ; METHOD-COMBINATION
        (set-q 4 +cdr-next+ dtp-fixnum #x81)             ; FLAGS
        (set-q 5 +cdr-next+ (tag-type ntag) ndata)       ; FLAVORS
        (set-q 6 +cdr-nil+ dtp-gf gf)))                  ; SELECTOR (self)
    ;; The (:TWO-PASS) method-combination list (keyword self-evaluation
    ;; comes free: cold-forward-all-keywords sweeps at finalize).
    (cw-set w mc (logior (ash +cdr-nil+ 6) dtp-symbol) two-pass)
    ;; (GET 'MAKE-INSTANCE 'GENERIC) = the object, the FIND-GENERIC-
    ;; FUNCTION interning read (defgeneric.lisp:340).
    (cold-prepend-property w mi-sym
                           (tag 0 dtp-symbol) generic-sym
                           (tag 0 dtp-gf) gf)
    (setf (getf (cold-world-machinery w) :make-instance-generic) gf)
    gf))

;;; ---------------- Generator-owned wired values ----------------

(defun cold-stamp-storage-values (w)
  "Values only the generator knows.  All writes go through the magic /
wired-table forwards, so they land in the comm slots or wired cells."
  (let ((fixnum (cold-dtp w "FIXNUM"))
        (locative (cold-dtp w "LOCATIVE")))
    (flet ((stamp (package name tag data)
             (cold-set-symbol-value w (make-vsym package name)
                                    (tag 0 tag) data)))
      ;; System 452.22, the distribution's cold version.
      (stamp "SYSTEM" "SYSCOM-MAJOR-VERSION-NUMBER" fixnum 452)
      (stamp "SYSTEM" "SYSCOM-MINOR-VERSION-NUMBER" fixnum 22)
      (stamp "SYSTEM" "%WIRED-PHYSICAL-ADDRESS-HIGH" fixnum
             (- +wired-zone-limit+ #xF8000000))
      (stamp "SYSTEM" "%WIRED-VIRTUAL-ADDRESS-HIGH" fixnum +wired-zone-limit+)
      (stamp "SYSTEM" "%CONTROL-STACK-LOW" locative #xF6000000)
      (stamp "SYSTEM" "FLOAT-OPERATION-STATUS" fixnum 0)
      (multiple-value-bind (tag data boundp)
          (cold-symbol-value-q
           w (make-vsym "SYSTEM" "*DEFAULT-FLOAT-OPERATING-MODE*"))
        (stamp "SYSTEM" "FLOAT-OPERATING-MODE" fixnum
               (if (and boundp (= (tag-type tag) fixnum)) data #x6C000000)))
      ;; HALT (storage/user-storage.lisp:444) reads CLI:*CONSOLES*
      ;; unguarded under WITHOUT-INTERRUPTS before halting -- the
      ;; mini-debugger's c-H trapped on it (post-M3h issue 6).  The
      ;; owner sys/console.lisp ((DEFVAR *CONSOLES* NIL), :113) is the
      ;; 50KB TV console flavor stack, QLD territory
      ;; (mini-alists.lisp:139) nothing else pre-banner needs; the
      ;; dist's saved value is NIL too.  Stamp the defvar's own initial
      ;; value instead of pulling the flavor stack cold.  (Second
      ;; R2-audit blind-spot instance, after *DATA-TYPE-NAME*.)
      (multiple-value-bind (ntag ndata) (cold-nil-q w)
        (cold-set-symbol-value
         w (make-vsym "COMMON-LISP-INTERNALS" "*CONSOLES*") ntag ndata))
      ;; Reserved-region identities (storage.lisp:249; nothing in the
      ;; sources sets these -- generator contract).  initialize-storage-
      ;; globals resets PAGE-TABLE-AREA's free pointer through them and
      ;; initialize-disk WIRED-DYNAMIC-AREA's, both pre-banner.
      (dolist (area-name '("WIRED-DYNAMIC-AREA" "PAGE-TABLE-AREA"
                           "GC-TABLE-AREA"))
        (let ((region (or (cold-area-current-region w area-name)
                          (error "no ~A region" area-name))))
          (stamp "STORAGE" (format nil "%~A-REGION" area-name) fixnum
                 (cold-region-number region))
          (stamp "STORAGE" (format nil "%~A-REGION-ORIGIN" area-name) fixnum
                 (cold-region-origin region))
          (stamp "STORAGE" (format nil "%~A-REGION-LENGTH" area-name) fixnum
                 (cold-region-length region))))
      ;; "Declare the area names to be variables (values are area numbers)
      ;; / These values are stored by the cold load generator now."
      ;; (ldata.lisp:152-153 -- explicit contract; M3h boot-11 trap:
      ;; first-boot GROW-DATA-STACK read STACK-AREA unbound).  Areas made
      ;; by load-time (SETQ var (MAKE-AREA ...)) forms are already bound
      ;; to the same number; a bound mismatch means the layout and the
      ;; load disagree and must be a hard error.
      (dotimes (i +cold-area-count+)
        (let* ((full-name (cold-area-name (cold-area w i)))
               (colon (or (position #\: full-name)
                          (error "unqualified area name ~A" full-name)))
               (vsym (make-vsym (subseq full-name 0 colon)
                                (subseq full-name (1+ colon)))))
          (multiple-value-bind (tag data boundp)
              (cold-symbol-value-q w vsym)
            (when (and boundp (not (and (= (tag-type tag) fixnum)
                                        (= data i))))
              (error "~A bound to ~2,'0X:~8,'0X, not area number ~D"
                     full-name tag data i)))
          (cold-set-symbol-value w vsym (tag 0 fixnum) i)))
      ;; Region allocator scalars (allocate-common.lisp:67-69): region
      ;; creation extends the tables at *NUMBER-OF-ACTIVE-REGIONS* and
      ;; pops *FREE-REGION*; nothing initializes either (M3h boot-5
      ;; trap: build-address-space-map's N-REGIONS loop read the count
      ;; unbound).  An empty free list is any value with bit 15 set
      ;; (REGION-VALID-P, storage.lisp:1564) -- NIL would trap in LDB.
      (stamp "SYSTEM-INTERNALS" "*NUMBER-OF-ACTIVE-REGIONS*" fixnum
             (fill-pointer (cold-world-regions w)))
      (stamp "SYSTEM-INTERNALS" "*FREE-REGION*" fixnum #xFFFF)
      ;; Allocator / GC / process scalars the safeguarded code reads
      ;; before anything sets them (M3h boot-9: C4-operand scan of the
      ;; F000xxxx code region).  All NIL/0 in the distribution except
      ;; the migration mode (ECASEd against SI:NORMAL) and the control
      ;; stack growth factor (single-float 1.3, dist verbatim).
      (multiple-value-bind (ntag ndata) (cold-nil-q w)
        (flet ((stamp-nil (package name)
                 (cold-set-symbol-value w (make-vsym package name)
                                        ntag ndata)))
          (stamp-nil "SYSTEM" "%STRUCTURE-CACHE-REGION")
          (stamp-nil "SYSTEM" "%LIST-CACHE-REGION")
          (stamp-nil "SYSTEM-INTERNALS" "*EPHEMERAL-GC-IN-PROGRESS*")
          (stamp-nil "SYMBOLICS-COMMON-LISP" "*CURRENT-PROCESS*")
          (stamp-nil "SYSTEM-INTERNALS" "GC-PROCESS")))
      (stamp "SYSTEM-INTERNALS" "GC-RECLAIM-OLDSPACE-INHIBIT" fixnum 0)
      (multiple-value-bind (st sd)
          (cold-symbol-ref w (make-vsym "SYSTEM-INTERNALS" "NORMAL"))
        (cold-set-symbol-value
         w (make-vsym "SYSTEM-INTERNALS" "*EPHEMERAL-MIGRATION-MODE*")
         st sd))
      (stamp "DEBUGGER" "PDL-GROW-RATIO" (cold-dtp w "SINGLE-FLOAT")
             #x3FA66666)
      (stamp "SYSTEM" "%REGION-CONS-ALARM" fixnum 0)
      (stamp "SYSTEM" "%PAGE-CONS-ALARM" fixnum 0)
      ;; Not SETQ'd anywhere in the cold set; ground truth has 8 (the
      ;; WORKING-STORAGE-AREA number) and T.
      (stamp "SYMBOLICS-COMMON-LISP" "*DEFAULT-CONS-AREA*" fixnum 8)
      (cold-set-symbol-value w (make-vsym "SYSTEM" "INHIBIT-SCHEDULING-FLAG")
                             (tag 0 (cold-dtp w "SYMBOL"))
                             (cold-world-t-vma w))
      (stamp "SYSTEM" "*LISP-RELEASE-STRING*" (cold-dtp w "STRING")
             (cold-string* w "Open Genera 2.0" "WIRED-CONTROL-TABLES")))))

;;; ---------------- Verbatim IFEP trap vectors ----------------

;;; Slots whose distribution Qs are constants of the IFEP/architecture, not
;;; products of the cold load: mode-3 FEP-mode handlers into the IFEP
;;; kernel (2095 = instruction-exception #o57, 2625 = %RESET-TRAP-VECTOR,
;;; 2631 = %FEP-MODE-TRAP-VECTOR) and the generic/message dispatch +
;;; fence-word Qs (2636-2639), whose targets are unmapped even in the
;;; distribution world.
(defparameter *cold-ifep-vector-slots* '(2095 2625 2631 2636 2637 2638 2639))

(defun cold-graft-ifep-vectors (w reference)
  (let ((base (cold-trap-base w)))
    (dolist (slot *cold-ifep-vector-slots*)
      (multiple-value-bind (tag data) (world-q reference (+ base slot))
        (unless tag
          (error "Reference trap slot ~D unmapped" slot))
        (cw-set w (+ base slot) tag data)))))

;;; ---------------- Verbatim IFEP FEPComm function slots ----------------

;;; FEPComm slots #x1F-#x31: IFEP debugger-kernel entry points the wired
;;; console path calls through (COLD-LOAD-STREAM-* etc.).  Their function
;;; cells are magic-forwarded into the block by sysdf1's
;;; DEFINE-MAGIC-LOCATIONS-1, and no cold file fdefines them -- the
;;; distribution generator grafted the kernel entries the same way.
;;; fepStartup (slot 2) deliberately stays non-1C so the emulator falls
;;; through to SYSCOM systemStartup (interfac.c:775).
(defconstant +cold-fepcomm-graft-start+ #x1F)
(defparameter *cold-fepcomm-graft-names*
  '("FEP-COMMAND-STRING" "FEP-CRASH-DATA-REQUEST"
    "COLD-LOAD-STREAM-READ-CHARACTER" "COLD-LOAD-STREAM-LISTEN"
    "COLD-LOAD-STREAM-READ-HARDWARE-CHARACTER"
    "COLD-LOAD-STREAM-DRAW-CHARACTER"
    "COLD-LOAD-STREAM-DISPLAY-LOZENGED-STRING" "COLD-LOAD-STREAM-SELECT"
    "COLD-LOAD-STREAM-BEEP" "COLD-LOAD-STREAM-FINISH"
    "COLD-LOAD-STREAM-INSIDE-SIZE" "COLD-LOAD-STREAM-SET-CURSORPOS"
    "COLD-LOAD-STREAM-READ-CURSORPOS" "COLD-LOAD-STREAM-COMPUTE-MOTION"
    "COLD-LOAD-STREAM-CLEAR-BETWEEN-CURSORPOSES"
    "COLD-LOAD-STREAM-SET-EDGES" "MAIN-SCREEN-PARAMETERS" "WIRED-FORMAT"
    "FEP-SEQUENCE-BREAK"))

(defun cold-fepcomm-block (w)
  "The FEP-COMMUNICATION-AREA layout block (name start end ventries)."
  (or (find-if (lambda (b)
                 (string= (strip-package (first b)) "FEP-COMMUNICATION-AREA"))
               (layout-section (cold-world-layout w) :magic-locations))
      (error "No FEP-COMMUNICATION-AREA in the layout")))

(defun cold-graft-fepcomm-functions (w reference)
  (destructuring-bind (name base end ventries) (cold-fepcomm-block w)
    (declare (ignore name end))
    (let ((dtp-cf (cold-dtp w "COMPILED-FUNCTION"))
          (dtp-null (cold-dtp w "NULL")))
      (loop for fname in *cold-fepcomm-graft-names*
            for slot from +cold-fepcomm-graft-start+
            for ventry = (nth slot ventries)
            do (destructuring-bind (vtype vval) ventry
                 ;; The slot must be the layout's function cell for FNAME.
                 (unless (and (eq vtype :function)
                              (string= (strip-package (second vval)) fname))
                   (error "FEPComm slot #x~X is ~S, expected FUNCTION ~A"
                          slot ventry fname)))
               ;; The load left the slot an unbound forwarded cell; a 1C
               ;; here would mean a cold file now defines it and the graft
               ;; would clobber that definition.
               (multiple-value-bind (tag data) (cw-ref w (+ base slot))
                 (declare (ignore data))
                 (unless (= (tag-type tag) dtp-null)
                   (error "FEPComm slot ~A already bound (~2,'0X)"
                          fname tag)))
               (multiple-value-bind (tag data)
                   (world-q reference (+ base slot))
                 (unless (and tag (= (tag-type tag) dtp-cf)
                              (<= #xF8010000 data #xF801FFFF))
                   (error "Reference FEPComm slot ~A is ~
~:[unmapped~;~:*~2,'0X:~8,'0X~], expected 1C:F801xxxx" fname tag data))
                 (cw-set w (+ base slot) tag data))))))

;;; ---------------- Generator-owned wired arrays ----------------

;;; Arrays the original generator allocated in WIRED-CONTROL-TABLES
;;; ("allocated/set up by the cold load generator" in the sources), all
;;; touched pre-banner: EMB-HANDLE-ARRAY-ALLOCATE carves handles out of
;;; *EMB-HANDLE-ARRAY* (emb-buffer.lisp:60; its DEFWIREDVAR initializer
;;; is #+IGNORE'd -- the M3h boot-2 trap at its instruction 2 proved the
;;; path); INITIALIZE-INTERRUPTS %block-stores *INTERRUPT-TASK-STORAGE*
;;; and INSTALL-EMB-SIGNAL-HANDLER the three EMB signal tables
;;; (interrupts.lisp:237,698; boot-3 trap); the wired scheduler pushes
;;; into *QUEUED-WAKEUPS* (wired.lisp:251); REINITIALIZE-OLDSPACE-MAP
;;; block-writes *OLDSPACE-MAP* (igc-cold.lisp:515; all-zero = nothing
;;; is oldspace, exactly right for a cold world); console attach
;;; indexes *SLB-WIRED-CONSOLES* (wired-console.lisp:253).  Lengths,
;;; leaders (fill pointers start 0) and packing are distribution ground
;;; truth (headers C0000A30 / C0000100 / C0000020 x3 / C000800A /
;;; A8000800 / C0008020).  *SLB-CONSOLE-BUFFER* also says "set up by
;;; cold-load generator" but is unbound even in the distribution.
;;; Spec: (package name type dims . options).  DIMS is a length or a
;;; dimension list (2-D arrays get the long-prefix format).  Options:
;;; :FILL-POINTER n / :LEADER-LENGTH n / :LEADER-LIST (..) shape the
;;; leader; :CONTENTS (..) bakes boxed elements (fixnums);
;;; :SYMBOL-CONTENTS (..) bakes boxed elements that are SYSTEM-package
;;; symbols named by the listed pname strings; :WORDS (..)
;;; bakes packed data words verbatim; :FILL-FIXNUM n fills boxed
;;; elements with a fixnum instead of NIL; :LAST-CDR-NIL sets the last
;;; element's cdr code (ART-Q-LIST list discipline).
(defparameter *cold-wired-arrays*
  '(("COMMON-LISP-INTERNALS" "*EMB-HANDLE-ARRAY*"       "ART-Q" 2608)
    ("COMMON-LISP-INTERNALS" "*INTERRUPT-TASK-STORAGE*" "ART-Q"  256)
    ("COMMON-LISP-INTERNALS" "*EMB-SIGNAL-HANDLER*"     "ART-Q"   32)
    ("COMMON-LISP-INTERNALS" "*EMB-SIGNAL-ARGUMENT*"    "ART-Q"   32)
    ("COMMON-LISP-INTERNALS" "*EMB-SIGNAL-PRIORITY*"    "ART-Q"   32)
    ("SYSTEM-INTERNALS"      "*QUEUED-WAKEUPS*"         "ART-Q"   10
     :fill-pointer 0)
    ("SYSTEM-INTERNALS"      "*OLDSPACE-MAP*"     "ART-BOOLEAN" 2048)
    ("SYSTEM-INTERNALS"      "*SLB-WIRED-CONSOLES*"     "ART-Q"   32
     :fill-pointer 0)
    ;; The M3h boot-4 batch: dist-array-valued wired variables with NO
    ;; allocation site in any source -- only the original generator can
    ;; have made them (found by sweeping every unbound fresh wired cell
    ;; against the distribution after boot 4 trapped on the unbound
    ;; *PENDING-INDICES* AREF in reinitialize-storage, pre-banner and
    ;; pre-ENABLE-TRAPPING, so it surfaced as AUX-HALT).
    ;; Demilevel 0 is level 1, all others -1 -- distribution verbatim.
    ("SYSTEM-INTERNALS" "*DEMILEVEL-LEVEL*"        "ART-8B"      64
     :words (#xFFFFFF01 #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
             #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
             #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
             #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF))
    ;; WIRED-FERROR fills this and treats it as a list (g-l-p).
    ("SYSTEM-INTERNALS" "WIRED-FERROR-ARGS-ARRAY"  "ART-Q-LIST"  10
     :fill-pointer 0 :last-cdr-nil t)
    ;; Mouse tracking state (wired-console.lisp:137-144,405): the scale
    ;; arrays are static acceleration tables -- distribution verbatim.
    ("TV" "MOUSE-CURSOR-PATTERN"                   "ART-1B"  (32 32))
    ("TV" "MOUSE-X-SCALE-ARRAY"                    "ART-FIXNUM"  16
     :contents (#x50 #x2AA #x7FFFFFFF #x555 #x7D0 #x7D0 #x7D0 #x7D0
                #x7D0 #x7D0 #x7D0 #x7D0 #x7D0 #x7D0 #x7D0 #x7D0))
    ("TV" "MOUSE-Y-SCALE-ARRAY"                    "ART-FIXNUM"  16
     :contents (#x50 #x266 #x7FFFFFFF #x4CC #x7D0 #x7D0 #x7D0 #x7D0
                #x7D0 #x7D0 #x7D0 #x7D0 #x7D0 #x7D0 #x7D0 #x7D0))
    ("TV" "MOUSE-BUTTONS-BUFFER"                   "ART-Q"       32)
    ("SYSTEM-INTERNALS" "*MOUSE-TIME-KBD-IN-POINTERS*" "ART-Q"   20
     :fill-pointer 20 :leader-length 2 :leader-list (nil 0))
    ;; Pager / storage wired state (storage.lisp:93,180-257):
    ;; reinitialize-storage AREFs *PENDING-INDICES* (boot-4 trap) and
    ;; *ZONE-COUNT-WIRED-PAGES* unconditionally.
    ("STORAGE" "*ECC-ERROR-LOG*"                   "ART-FIXNUM" 200
     :fill-fixnum 0)
    ("STORAGE" "*PREFETCH-FRAMES*"                 "ART-Q"       64)
    ("STORAGE" "*PENDING-VPN*"                     "ART-Q"       83)
    ("STORAGE" "*PENDING-INDICES*"                 "ART-Q"       83)
    ("STORAGE" "*READ-ONLY-WRITTEN-PAGES*"         "ART-Q"        8)
    ("STORAGE" "*ZONE-COUNT-WIRED-PAGES*"          "ART-Q"       32
     :fill-fixnum 0)
    ("STORAGE" "*PAGE-WAITER-VPN*"                 "ART-Q"        8)
    ("STORAGE" "*PAGE-WAITER-PROCESS*"             "ART-Q"        8)
    ;; Level -> section-type policy nibbles (allocate-common.lisp:86,
    ;; safeguarded; M3h boot-8: FERROR's consing hit the allocator,
    ;; which reads it -- the error recursed 20 deep and AUX-HALTed).
    ;; Distribution verbatim: levels 0-3 ephemeral (5), 32-36 the
    ;; static/dynamic ladder (1,2,3,4,3) -- covers levels 2/33/35 that
    ;; this world's region bits use.
    ("SYSTEM-INTERNALS" "*LEVEL-TYPE*"             "ART-4B"      64
     :area "SAFEGUARDED-OBJECTS-AREA"
     :words (#x00005555 0 0 0 #x00034321 0 0 0))
    ;; Ephemeral level -> PHT group crumbs (i-allocate.lisp:93, the
    ;; third of the "initialized by the cold load generator" trio with
    ;; *ZONE-LEVEL* / *DEMILEVEL-LEVEL*).  Distribution verbatim.
    ("SYSTEM-INTERNALS" "*EPHEMERAL-LEVEL-GROUP*"  "ART-2B"      32
     :area "SAFEGUARDED-OBJECTS-AREA"
     :words (#x79E79E71 #x9E79E79E))
    ;; Flip thresholds for the in-use ephemeral levels 0-3 (gc-defs.lisp
    ;; :352 -- gc-defs is NOT in the cold set, so its DEFVAR initializer
    ;; never runs; MAKE-REGION-INTERNAL %FIXNUM-CEILINGs the level's
    ;; entry when consing makes an ephemeral region -- M3h boot-9's
    ;; recursion driver).  Values are the distribution's.
    ("SYSTEM-INTERNALS" "*EPHEMERAL-GC-FLIP-CAPACITY*" "ART-Q"   32
     :area "SAFEGUARDED-OBJECTS-AREA"
     :contents (#x186A0 #x30D40 #x4E20 #x2710))
    ;; Data-type code -> name symbol, the DEFENUMERATED *DATA-TYPES*
    ;; order (i-sys/sysdef.lisp:166).  sysdef.lisp:155: "Patch
    ;; *DATA-TYPE-NAME*, set up by from *DATA-TYPES* by the cold-load
    ;; generator"; its DEFVAR-SAFEGUARDED (sys/ldata.lisp:224) has no
    ;; Lisp initializer.  DATA-TYPE-NAME is this array applied as a
    ;; function (sys2/macro.lisp:1018) and PRINT-OBJECT's
    ;; error-recovery / random-object fallbacks call it for any object
    ;; PRINT can't dispatch (io/print.lisp:204,242,279,284), so leaving
    ;; it unbound turned EVERY post-banner error report into a trap-71
    ;; recursion on the safeguarded cell (first post-M3h defect).
    ;; Distribution verbatim: leaderless ART-Q 64 in
    ;; SAFEGUARDED-OBJECTS-AREA (header C0000040 at F0001466).
    ("SYSTEM-INTERNALS" "*DATA-TYPE-NAME*"          "ART-Q"       64
     :area "SAFEGUARDED-OBJECTS-AREA"
     :symbol-contents
     ("DTP-NULL" "DTP-MONITOR-FORWARD" "DTP-HEADER-P" "DTP-HEADER-I"
      "DTP-EXTERNAL-VALUE-CELL-POINTER" "DTP-ONE-Q-FORWARD"
      "DTP-HEADER-FORWARD" "DTP-ELEMENT-FORWARD"
      "DTP-FIXNUM" "DTP-SMALL-RATIO" "DTP-SINGLE-FLOAT" "DTP-DOUBLE-FLOAT"
      "DTP-BIGNUM" "DTP-BIG-RATIO" "DTP-COMPLEX" "DTP-SPARE-NUMBER"
      "DTP-INSTANCE" "DTP-LIST-INSTANCE" "DTP-ARRAY-INSTANCE"
      "DTP-STRING-INSTANCE"
      "DTP-NIL" "DTP-LIST" "DTP-ARRAY" "DTP-STRING" "DTP-SYMBOL"
      "DTP-LOCATIVE" "DTP-LEXICAL-CLOSURE" "DTP-DYNAMIC-CLOSURE"
      "DTP-COMPILED-FUNCTION" "DTP-GENERIC-FUNCTION"
      "DTP-SPARE-POINTER-1" "DTP-SPARE-POINTER-2"
      "DTP-PHYSICAL-ADDRESS" "DTP-SPARE-IMMEDIATE-1" "DTP-BOUND-LOCATION"
      "DTP-CHARACTER" "DTP-LOGIC-VARIABLE" "DTP-GC-FORWARD"
      "DTP-EVEN-PC" "DTP-ODD-PC"
      "DTP-CALL-COMPILED-EVEN" "DTP-CALL-COMPILED-ODD"
      "DTP-CALL-INDIRECT" "DTP-CALL-GENERIC"
      "DTP-CALL-COMPILED-EVEN-PREFETCH" "DTP-CALL-COMPILED-ODD-PREFETCH"
      "DTP-CALL-INDIRECT-PREFETCH" "DTP-CALL-GENERIC-PREFETCH"
      "DTP-PACKED-INSTRUCTION-60" "DTP-PACKED-INSTRUCTION-61"
      "DTP-PACKED-INSTRUCTION-62" "DTP-PACKED-INSTRUCTION-63"
      "DTP-PACKED-INSTRUCTION-64" "DTP-PACKED-INSTRUCTION-65"
      "DTP-PACKED-INSTRUCTION-66" "DTP-PACKED-INSTRUCTION-67"
      "DTP-PACKED-INSTRUCTION-70" "DTP-PACKED-INSTRUCTION-71"
      "DTP-PACKED-INSTRUCTION-72" "DTP-PACKED-INSTRUCTION-73"
      "DTP-PACKED-INSTRUCTION-74" "DTP-PACKED-INSTRUCTION-75"
      "DTP-PACKED-INSTRUCTION-76" "DTP-PACKED-INSTRUCTION-77"))))

(defun cold-build-wired-arrays (w)
  (dolist (spec *cold-wired-arrays*)
    (destructuring-bind (package name type dims
                         &key fill-pointer leader-length leader-list
                              contents symbol-contents words fill-fixnum
                              last-cdr-nil
                              (area "WIRED-CONTROL-TABLES"))
        spec
      (let* ((len (if (listp dims) (reduce #'* dims) dims))
             (arr (make-varray
                   (if (listp dims) dims (list dims))
                   (append
                    (list (make-vsym "KEYWORD" "TYPE")
                          (make-vsym "SYSTEM" type))
                    (when fill-pointer
                      (list (make-vsym "KEYWORD" "FILL-POINTER")
                            fill-pointer))
                    (when leader-length
                      (list (make-vsym "KEYWORD" "LEADER-LENGTH")
                            leader-length))
                    (when leader-list
                      (list (make-vsym "KEYWORD" "LEADER-LIST")
                            leader-list))))))
        (when contents
          ;; Shorter than the array: the tail stays NIL.
          (setf (varray-contents arr)
                (coerce (append contents
                                (make-list (- len (length contents))))
                        'vector)))
        (when symbol-contents
          (setf (varray-contents arr)
                (coerce (append (mapcar (lambda (pname)
                                          (make-vsym "SYSTEM" pname))
                                        symbol-contents)
                                (make-list (- len (length symbol-contents))))
                        'vector)))
        (let* ((hdr (cold-array w arr area))
               (fixnum (cold-dtp w "FIXNUM")))
          (when words
            (loop for word in words
                  for vma from (+ hdr 1)
                  do (cw-set w vma (tag 0 fixnum) word)))
          (when fill-fixnum
            (dotimes (i len)
              (cw-set w (+ hdr 1 i) (tag 0 fixnum) fill-fixnum)))
          (when last-cdr-nil
            (multiple-value-bind (tag data) (cw-ref w (+ hdr len))
              (cw-set w (+ hdr len) (logior #x40 tag) data)))
          (cold-set-symbol-value w (make-vsym package name)
                                 (tag 0 (cold-dtp w "ARRAY")) hdr))))))

;;; ---------------- Allocator and stack-registry tables ----------------

(defconstant +cold-zone-count+ 32)

(defun cold-build-zone-level (w)
  "SI:*ZONE-LEVEL* -- \"initialized by the cold load generator\"
(i-allocate.lisp:90): each zone's byte is the level of its resident
regions, -1 elsewhere; region creation validates new regions against it
(M3h boot-9 original trap at PC F0001B99)."
  (let ((bytes (make-array +cold-zone-count+ :initial-element #xFF)))
    (loop for region across (cold-world-regions w)
          for zone = (ldb (byte 5 27) (cold-region-origin region))
          for level = (ldb (byte 6 18) (cold-region-bits-for w region))
          do (let ((old (aref bytes zone)))
               (unless (member old (list #xFF level))
                 (error "Zone ~D hosts levels ~D and ~D" zone old level))
               (setf (aref bytes zone) level)))
    (let* ((arr (make-varray (list +cold-zone-count+)
                             (list (make-vsym "KEYWORD" "TYPE")
                                   (make-vsym "SYSTEM" "ART-8B"))))
           (hdr (cold-array w arr "SAFEGUARDED-OBJECTS-AREA"))
           (fixnum (cold-dtp w "FIXNUM")))
      (dotimes (word (/ +cold-zone-count+ 4))
        (cw-set w (+ hdr 1 word) (tag 0 fixnum)
                (loop for b below 4
                      sum (ash (aref bytes (+ (* word 4) b)) (* 8 b)))))
      (cold-set-symbol-value w (make-vsym "SYSTEM-INTERNALS" "*ZONE-LEVEL*")
                             (tag 0 (cold-dtp w "ARRAY")) hdr))))

(defun cold-build-stack-registry (w)
  "The safeguarded stack tables (ldata.lisp:215-218): FIND-STACK
binary-searches origin/length/stack-group triples sorted ascending,
bounded by *NUMBER-OF-ACTIVE-STACKS*.  The fresh world registers the
initial stack group's binding and control stacks, which
cold-build-initial-stack-group allocates at the architectural region
starts (cold-wired.lisp:51-52)."
  (let* ((fixnum (cold-dtp w "FIXNUM"))
         (array (cold-dtp w "ARRAY"))
         (sg (cold-machinery w :initial-stack-group))
         (stacks `((#xF2000000 ,+cold-binding-stack-size+)
                   (#xF6000000 ,+cold-control-stack-size+))))
    (flet ((build (name type)
             (let* ((arr (make-varray (list 256)
                                      (list (make-vsym "KEYWORD" "TYPE")
                                            (make-vsym "SYSTEM" type))))
                    (hdr (cold-array w arr "SAFEGUARDED-OBJECTS-AREA")))
               (cold-set-symbol-value w (make-vsym "SYSTEM-INTERNALS" name)
                                      (tag 0 array) hdr)
               hdr)))
      (let ((org (build "*STACK-ORIGIN*" "ART-FIXNUM"))
            (len (build "*STACK-LENGTH*" "ART-FIXNUM"))
            (ssg (build "*STACK-STACK-GROUP*" "ART-Q")))
        (dotimes (i 256)
          (cw-set w (+ org 1 i) (tag 0 fixnum) 0)
          (cw-set w (+ len 1 i) (tag 0 fixnum) 0))
        (loop for (origin size) in stacks
              for i from 0
              do (cw-set w (+ org 1 i) (tag 0 fixnum) origin)
                 (cw-set w (+ len 1 i) (tag 0 fixnum) size)
                 (cw-set w (+ ssg 1 i) (tag 0 array) sg))
        (cold-set-symbol-value w (make-vsym "SYSTEM-INTERNALS"
                                            "*NUMBER-OF-ACTIVE-STACKS*")
                               (tag 0 fixnum) (length stacks))))))

;;; Distribution patterns for the array metadata (ldata.lisp:213-223):
;;; the null WORD is fixnum 0 for the numeric/packed types, and the null
;;; ELEMENT is 0 for numerics and the null character for strings.
(defparameter *cold-array-null-word-zeros* '(0 2 4 6 8 10 16 20 42))
(defparameter *cold-array-null-element-zeros* '(0 2 4 6 8 10))
(defparameter *cold-array-null-element-chars* '(16 20))

(defun cold-build-array-meta (w)
  "SYS:*ARRAY-TYPES* / *ARRAY-NULL-WORD* / *ARRAY-NULL-ELEMENT* /
*VALID-ARRAY-TYPE-CODES* (safeguarded, ldata.lisp): the type-code ->
name-symbol map (known names at their codes, %ARRAY-TYPE-~O filler like
the distribution), the per-type null Qs, and the defined-type bitmap.
*ARRAY-ELEMENTS-PER-Q* / *ARRAY-BITS-PER-ELEMENT* stay unbound -- they
are unbound in the distribution too."
  (let ((fixnum (cold-dtp w "FIXNUM"))
        (array (cold-dtp w "ARRAY"))
        (char (cold-dtp w "CHARACTER")))
    (multiple-value-bind (ntag ndata) (cold-nil-q w)
      (flet ((build (name fill-fn)
               (let* ((arr (make-varray (list 64)
                                        (list (make-vsym "KEYWORD" "TYPE")
                                              (make-vsym "SYSTEM" "ART-Q"))))
                      (hdr (cold-array w arr "SAFEGUARDED-OBJECTS-AREA")))
                 (dotimes (i 64) (funcall fill-fn (+ hdr 1 i) i))
                 (cold-set-symbol-value w (make-vsym "SYSTEM" name)
                                        (tag 0 array) hdr))))
        (build "*ARRAY-TYPES*"
               (lambda (vma i)
                 (let ((name (or (car (rassoc i *cold-array-type-codes*))
                                 (format nil "%ARRAY-TYPE-~O" i))))
                   (multiple-value-bind (st sd)
                       (cold-symbol-ref w (make-vsym "SYSTEM" name))
                     (cw-set w vma st sd)))))
        (build "*ARRAY-NULL-WORD*"
               (lambda (vma i)
                 (if (member i *cold-array-null-word-zeros*)
                     (cw-set w vma (tag 0 fixnum) 0)
                     (cw-set w vma ntag ndata))))
        (build "*ARRAY-NULL-ELEMENT*"
               (lambda (vma i)
                 (cond ((member i *cold-array-null-element-zeros*)
                        (cw-set w vma (tag 0 fixnum) 0))
                       ((member i *cold-array-null-element-chars*)
                        (cw-set w vma (tag 0 char) 0))
                       (t (cw-set w vma ntag ndata)))))
        ;; *VALID-ARRAY-TYPE-CODES*: an ART-BOOLEAN 64-bitmap with a bit set
        ;; for every defined array type code (SIMPLE-MAKE-ARRAY-TYPE-AREA
        ;; icons.lisp:1597 truth-tests (AREF ... TYPE)).  The dist header is
        ;; 43:A8000040 and its two data words 00110555/00030400 are exactly
        ;; the *cold-array-type-codes* set, so compute the bitmap from it.
        (let* ((bits (reduce (lambda (v pair) (logior v (ash 1 (cdr pair))))
                             *cold-array-type-codes* :initial-value 0))
               (arr (make-varray (list 64)
                                 (list (make-vsym "KEYWORD" "TYPE")
                                       (make-vsym "SYSTEM" "ART-BOOLEAN")))))
          (setf (varray-words arr)
                (make-array 4 :initial-contents
                            (list (ldb (byte 16 0) bits) (ldb (byte 16 16) bits)
                                  (ldb (byte 16 32) bits) (ldb (byte 16 48) bits))))
          (cold-set-symbol-value
           w (make-vsym "SYSTEM" "*VALID-ARRAY-TYPE-CODES*")
           (tag 0 array) (cold-array w arr "SAFEGUARDED-OBJECTS-AREA")))))))

;;; ---------------- System disk events ----------------

;;; disk-driver.lisp:93: "The following `system' disk events are
;;; allocated in wired-control-tables by the cold load generator" -- 18-Q
;;; DISK-EVENT defstorages (disk-definitions.lisp:104, IMach form: ()
;;; header + () named-structure-symbol + 16 fields).  initialize-disk
;;; (pre-banner on VLM) only wires *ROOT-DISK-EVENT* up via
;;; initialize-system-disk-event, so generator-fresh events are all-NIL;
;;; only the root carries the named-structure bit and its DISK-EVENT
;;; symbol (dist headers C2000012 / C0000012).  The serial event IS the
;;; storage root ("the serial disk event is the root"): both variables
;;; reference one object, so there are five variables but four events.
(defun cold-build-disk-events (w)
  (multiple-value-bind (ntag ndata) (cold-nil-q w)
    (flet ((event (&key root)
             (let ((hdr (cold-reserve-wired-array
                         w "WIRED-CONTROL-TABLES" "ART-Q" 18
                         :named-structure root)))
               (loop for i from 1 to 18
                     do (cw-set w (+ hdr i) ntag ndata))
               (when root
                 (multiple-value-bind (st sd)
                     (cold-symbol-ref w (make-vsym "STORAGE" "DISK-EVENT"))
                   (cw-set w (+ hdr 1) st sd)))
               hdr)))
      (let ((root (event :root t))
            (serial (event))
            (parallel (event))
            (background (event))
            (array (cold-dtp w "ARRAY")))
        (loop for (name hdr) in `(("*ROOT-DISK-EVENT*" ,root)
                                  ("*STORAGE-ROOT-DISK-EVENT*" ,serial)
                                  ("*STORAGE-SERIAL-DISK-EVENT*" ,serial)
                                  ("*STORAGE-PARALLEL-DISK-EVENT*" ,parallel)
                                  ("*STORAGE-BACKGROUND-DISK-EVENT*" ,background))
              do (cold-set-symbol-value w (make-vsym "STORAGE" name)
                                        (tag 0 array) hdr))))))

;;; ---------------- pre-banner IGNORE aliases ----------------

(defparameter *cold-ignore-stub-functions*
  '(("FLAVOR" . "PRINT-FLAVOR-TRANSFORMATION-WARNINGS")
    ("FLAVOR" . "COMPOSE-INITIALIZATIONS")
    ("FLAVOR" . "VALIDATE-CONSTRUCTOR-FUNCTIONS"))
  "M3h boot-33 review: warm flavor/make.lisp functions the COLD flavor
runtime calls unconditionally during the deferred flavor phase.  Genera's
own *COLD-LOAD-FUNCTION-INITIALIZATIONS* (cold-load.lisp:131) builds the
FSET-stub environment QLD loads flavor files in -- DW type-redefinition
IGNOREs, DEFGENERIC-INTERNAL-COLD, MAKE-INSTANCE-COLD -- but it never
needed rows for these three because QLD loads SYS: FLAVOR; MAKE near the
front of INNER-SYSTEM-FILE-ALIST (mini-alists.lisp:91), before any file
whose defflavors run.  Our deferred list runs every flavor form before
any QLD, so the gap bites:
  PRINT-FLAVOR-TRANSFORMATION-WARNINGS -- WITH-TRANSFORM-FLAVOR-WARNINGS'
    UNWIND-PROTECT cleanup (defflavor.lisp:389-394) calls it from
    DEFFLAVOR-INTERNAL-1's new-flavor arm; *TRANSFORM-FLAVOR-WARNINGS*
    is always NIL pre-banner (no instances -> no transforms), so the
    real function would no-op.
  COMPOSE-INITIALIZATIONS -- COMPILE-FLAVOR-METHODS-INITIALIZATIONS
    (compose.lisp:1080) for every non-abstract CFM'd flavor.  Stubbing
    leaves FLAVOR-INITIALIZATIONS-COMPOSED false, which also keeps the
    defmethod-side VALIDATE-CONSTRUCTOR-FUNCTIONS guards
    (defmethod.lisp:1070,1164) false; warm QLD reload / first real
    instantiation composes lazily.
  VALIDATE-CONSTRUCTOR-FUNCTIONS -- COMPILE-FLAVOR-METHODS-LOAD-TIME's
    unconditional constructor pass (compose.lisp:1075); constructors
    only matter to MAKE-INSTANCE, itself stubbed to marker lists.
QLD's flavor/make load FDEFINEs the real definitions over these cells,
the same shadowing the dist shows (its PRINT-FLAVOR-TRANSFORMATION-
WARNINGS fcell forwards into the QLD band, 05:82201B63).  The graft
errors out if a name becomes fbound: an entry must be dropped when its
file joins the cold set (the boot-6 emb-ethernet discipline).")

(defun cold-graft-ignore-stubs (w)
  (let ((dtp-cf (cold-dtp w "COMPILED-FUNCTION"))
        (dtp-null (cold-dtp w "NULL"))
        (ignore-cell (cold-follow-cell
                      w (+ (cold-vsym w (make-vsym "LISP" "IGNORE")) 2))))
    (multiple-value-bind (itag idata) (cw-ref w ignore-cell)
      (unless (= (tag-type itag) dtp-cf)
        (error "LISP:IGNORE is not fbound (~2,'0X:~8,'0X)" itag idata))
      (loop for (pkg . name) in *cold-ignore-stub-functions*
            do (let ((cell (cold-follow-cell
                            w (cold-fdefinition-cell
                               w (make-vsym pkg name)))))
                 (multiple-value-bind (tag data) (cw-ref w cell)
                   (declare (ignore data))
                   (unless (= (tag-type tag) dtp-null)
                     (error "~A:~A is defined now -- drop its IGNORE stub"
                            pkg name))
                   (cw-set w cell (logior (logand tag #xC0) dtp-cf)
                           idata)))))))

;;; ---------------- FEP boot parameters ----------------

;;; FEPComm slots the FEP populates on real hardware before starting
;;; Lisp, so no cold file sets them, but the world must carry them: the
;;; IFEP kernel reads them during its startup go/no-go and HALTS the
;;; machine (silently, pre-console) when they are unbound -- proved
;;; empirically 2026-07-04 by stamping a halting fresh.ilod into one the
;;; IFEP boots.  *SYSTEM-TYPE* is also checked by
;;; INITIALIZE-EMB-COMM-AREA against the host's EMB area type, and
;;; *EMB-COMMUNICATION-AREA* equals what the host puts in BOOT-COMM slot
;;; 0.  Values = distribution ground truth Qs (genera-8-5-wired.txt).
(defparameter *cold-fepcomm-boot-stamps*
  ;; (name tag-name data), all SYSTEM:; slot = layout ventry position
  '(("*SYSTEM-TYPE*"                 "FIXNUM"   #x0000020E)
    ("%FEP-PHYSICAL-ADDRESS-HIGH"    "FIXNUM"   #x00040000)
    ("%UNWIRED-VIRTUAL-ADDRESS-LOW"  "FIXNUM"   0)
    ("%UNWIRED-VIRTUAL-ADDRESS-HIGH" "FIXNUM"   0)
    ("%UNWIRED-PHYSICAL-ADDRESS-LOW" "FIXNUM"   0)
    ("%UNWIRED-PHYSICAL-ADDRESS-HIGH" "FIXNUM"  0)
    ("*REQUESTING-LISP-TO-STOP*"     "NIL"      :nil)
    ("%SOFTWARE-CONFIGURATION"       "FIXNUM"   #x0000000F)
    ("*EMB-COMMUNICATION-AREA*"      "LOCATIVE" #xFFFE0080)))

(defun cold-stamp-fepcomm-boot-slots (w)
  (dolist (s *cold-fepcomm-boot-stamps*)
    (destructuring-bind (name tag-name data) s
      (cold-set-symbol-value
       w (make-vsym "SYSTEM" name)
       (tag 0 (cold-dtp w tag-name))
       (if (eq data :nil) (cold-world-nil-vma w) data)))))

;;; ---------------- Driver ----------------

(defun cold-build-wired-machinery (w &key reference)
  "M3e pass, after the cold set has loaded (the magic-location forwarding
already ran during the load).  Builds the initial stack group, stamps the
generator-owned wired values, fills the storage tables from the final
region state, and grafts the IFEP trap vectors and FEPComm function
slots from the reference world."
  (cold-build-initial-stack-group w)
  (cold-build-stack-grower w)
  (cold-stamp-storage-values w)
  ;; These allocate in WIRED-CONTROL-TABLES / SAFEGUARDED-OBJECTS-AREA
  ;; -- they must precede the table fill so the region free pointers
  ;; cover them.
  (cold-build-wired-arrays w)
  (cold-build-disk-events w)
  (cold-build-zone-level w)
  (cold-build-stack-registry w)
  (cold-build-array-meta w)
  ;; Allocates in *FLAVOR-STATIC-AREA* + PROPERTY-LIST-AREA: also before
  ;; the table fill (M3h boot 36).
  (cold-build-make-instance-generic w)
  (cold-fill-storage-tables w :reference reference)
  (cold-stamp-fepcomm-boot-slots w)
  (cold-graft-ignore-stubs w)
  (when reference
    (cold-graft-ifep-vectors w reference)
    (cold-graft-fepcomm-functions w reference))
  w)
