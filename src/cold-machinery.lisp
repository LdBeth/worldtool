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

(defconstant +cold-area-count+ 22)

;;; Distribution REGION-BITS for the architectural regions 0-5 (probe).
(defparameter *cold-architectural-region-bits*
  #(#x00800008 #x00800049 #x02840049 #x02840068 #x0284006C #x02840060))

;;; %%REGION- fields (layout DEFSYSBYTES): representation bits 0-1,
;;; space-type bits 2-5, scavenge-enable bit 6, level bits 18-23.
(defconstant +region-bits-scavenge+ (ash 1 6))

(defun cold-heap-region-bits (area-bits)
  "REGION-BITS for a generator heap region: the area template with the
representation forced to STRUCTURE and an ephemeral-level template (level
< 32, e.g. WORKING-STORAGE-AREA's level-1 marker) replaced by plain
dynamic level 35 -- the cold world boots with the ephemeral GC off."
  (let ((bits (logior (logandc2 area-bits 3) 1)))
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

(defun cold-region-bits-for (w region)
  (let ((n (cold-region-number region))
        (area (cold-region-area region)))
    (cond ((< n (length *cold-architectural-region-bits*))
           (aref *cold-architectural-region-bits* n))
          (t
           (let ((config (assoc area *cold-area-config*)))
             (unless config
               (error "Region ~D belongs to non-cold area ~D" n area))
             (if (member area '(16 17))          ; PAGE-TABLE / GC-TABLE
                 (third config)
                 (cold-heap-region-bits (third config)))))
    )))

(defun cold-region-has-pages-p (w region)
  "Does any world page fall inside the region?  (FEP-AREA and reserved
address space like PAGE-TABLE-AREA have none.)"
  (loop for vma from (cold-region-origin region)
          below (cold-region-free region) by +ivory-page-size-qs+
        thereis (nth-value 2 (cw-ref w vma))))

(defun cold-fill-storage-tables (w)
  "Write the wired region tables and safeguarded area/oblast tables from
this world's areas and regions."
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
            (cold-table-set16 w rlist-tbl a
                              (let ((area (and config (aref (cold-world-areas w) a))))
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
        (dolist (tbl (list name-tbl maxq-tbl rqsz-tbl rlist-tbl abits-tbl))
          (cold-set-fill-pointer w tbl +cold-area-count+)))
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
  "Take SIZE Qs from the area's architectural region (must be the region
start) and make the pages present.  Returns the base vma."
  (let* ((region (cold-area-current-region w area-name))
         (base (cold-region-free region)))
    (unless (= base (cold-region-origin region))
      (error "~A stack must be the region's first allocation" area-name))
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
      ;; Region allocator scalars (allocate-common.lisp:67-69): region
      ;; creation extends the tables at *NUMBER-OF-ACTIVE-REGIONS* and
      ;; pops *FREE-REGION*; nothing initializes either (M3h boot-5
      ;; trap: build-address-space-map's N-REGIONS loop read the count
      ;; unbound).  An empty free list is any value with bit 15 set
      ;; (REGION-VALID-P, storage.lisp:1564) -- NIL would trap in LDB.
      (stamp "SYSTEM-INTERNALS" "*NUMBER-OF-ACTIVE-REGIONS*" fixnum
             (fill-pointer (cold-world-regions w)))
      (stamp "SYSTEM-INTERNALS" "*FREE-REGION*" fixnum #xFFFF)
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
;;; leader; :CONTENTS (..) bakes boxed elements (fixnums); :WORDS (..)
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
    ("STORAGE" "*PAGE-WAITER-PROCESS*"             "ART-Q"        8)))

(defun cold-build-wired-arrays (w)
  (dolist (spec *cold-wired-arrays*)
    (destructuring-bind (package name type dims
                         &key fill-pointer leader-length leader-list
                              contents words fill-fixnum last-cdr-nil)
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
          (setf (varray-contents arr) (coerce contents 'vector)))
        (let* ((hdr (cold-array w arr "WIRED-CONTROL-TABLES"))
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

;;; ---------------- Missing-source stubs ----------------

;;; emb-ethernet-driver.lisp was in the original cold set -- the
;;; distribution has both these functions wired (1C:F80492xx) and
;;; initialize-disk's VLM branch calls INITIALIZE-EMBEDDED-NETWORK
;;; pre-banner (M3h boot-6 trap: unbound fn cell called from wired
;;; disk init) -- but no .vbin of it survives on disk.  Until the file
;;; is recompiled into the set, alias both entries to IGNORE (&rest,
;;; returns NIL), the same aliasing the cold load's own
;;; *COLD-LOAD-FUNCTION-INITIALIZATIONS* uses (cold-load.lisp:131,
;;; e.g. LT:FUNCTION-INLINE-FORM-METHOD . IGNORE).  Consequences: no
;;; EMB network interfaces, and the periodic timer's Embedded branch
;;; (interrupts.lisp:137,800) is a no-op; the banner path needs no
;;; networking.  DROP both stubs when the real .vbin joins the set --
;;; the graft errors if the name is already defined.
(defparameter *cold-ignore-stub-names*
  '("INITIALIZE-EMBEDDED-NETWORK" "EMB-ETHERNET-PERIODIC-TIMER-FUNCTION"))

(defun cold-graft-ignore-stubs (w)
  (let ((dtp-cf (cold-dtp w "COMPILED-FUNCTION"))
        (dtp-null (cold-dtp w "NULL"))
        (ignore-cell (cold-follow-cell
                      w (+ (cold-vsym w (make-vsym "LISP" "IGNORE")) 2))))
    (multiple-value-bind (itag idata) (cw-ref w ignore-cell)
      (unless (= (tag-type itag) dtp-cf)
        (error "LISP:IGNORE is not fbound (~2,'0X:~8,'0X)" itag idata))
      (dolist (name *cold-ignore-stub-names*)
        (let ((cell (cold-follow-cell
                     w (cold-fdefinition-cell
                        w (make-vsym "NETWORK-INTERNALS" name)))))
          (multiple-value-bind (tag data) (cw-ref w cell)
            (declare (ignore data))
            (unless (= (tag-type tag) dtp-null)
              (error "NETI:~A is defined now -- drop its IGNORE stub"
                     name))
            (cw-set w cell (logior (logand tag #xC0) dtp-cf) idata)))))))

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
  (cold-stamp-storage-values w)
  ;; These allocate in WIRED-CONTROL-TABLES -- they must precede the
  ;; table fill so the region free pointers cover them.
  (cold-build-wired-arrays w)
  (cold-build-disk-events w)
  (cold-fill-storage-tables w)
  (cold-stamp-fepcomm-boot-slots w)
  (cold-graft-ignore-stubs w)
  (when reference
    (cold-graft-ifep-vectors w reference)
    (cold-graft-fepcomm-functions w reference))
  w)
