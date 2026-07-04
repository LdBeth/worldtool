;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Cold-load generator: in-memory image under construction.
;;;
;;; A cold-world is a sparse VMA space of Qs (256-Q Ivory pages on demand)
;;; plus the bookkeeping the generator threads through every stage: areas
;;; and regions (mirroring SYS:SYS;LDATA the cold-load-created set), interned
;;; symbols, function definitions, build-time variable values, deferred
;;; first-boot forms, and forward-reference fixups.  The layout tables
;;; (cold-layout.sexp) are the single source for type codes, architectural
;;; addresses, and structure offsets.

(in-package #:worldtool)

;;; Regions and areas.  Region numbers index the wired region tables the
;;; finished world carries (SYSCOM *REGION-* arrays, built in cold-wired).
(defstruct cold-region
  (number 0) (area 0)
  (origin 0)       ; base VMA
  (length 0)       ; capacity in Qs
  (free 0))        ; next unallocated VMA

(defstruct cold-area
  (number 0) (name "") (regions nil))   ; region numbers, newest first

(defstruct cold-world
  (layout nil)
  (pages (make-hash-table))              ; (ash vma -8) -> 256-Q qvec
  (areas (make-array 64 :initial-element nil))
  (regions (make-array 64 :adjustable t :fill-pointer 0))
  (dtp-cache (make-hash-table :test #'equal))
  ;; Filled by later stages:
  (symbols (make-hash-table :test #'equal)) ; (pname . package) -> symbol vma
  (strings (make-hash-table :test #'equal)) ; pname string -> string vma
  (fdefs (make-hash-table :test #'equal))   ; fspec key -> function vma
  (values (make-hash-table :test #'equal))  ; symbol key -> build-time value
  (plists (make-hash-table :test #'equal))  ; symbol key -> build-time plist
  (deferred nil)                            ; reversed *COLD-LOAD-DEFERRED-FORMS*,
                                            ; entries (package-string . host-form)
  (fixups nil)                              ; thunks re-run until quiescent at finalize
  (patches nil)                             ; (vma package-string form): first-boot
                                            ; %P-STORE-CONTENTS patches for Qs whose
                                            ; value only exists at run time
  (magic nil)                               ; DEFINE-MAGIC-LOCATIONS-1 stash (M3e
                                            ; gate cross-check; the forwarding
                                            ; itself happens at load time)
  (machinery nil)                           ; plist: reserved wired/storage-table
                                            ; header vmas + :initial-stack-group
  ;; DECLARE-STORAGE-CATEGORY-LOAD wired-cell forwarding (cold-eval):
  (wired-cell-table 0)                      ; FORWARDED-SYMBOL-CELL-TABLE header vma
  (wired-cell-fill 0)                       ; its fill pointer
  ;; Landmarks stamped by cold-wired:
  (nil-vma 0) (t-vma 0) (catch-all-pc 0)
  ;; MAKE-INSTANCE-COLD marker (cold-load.lisp:404): instances in the cold
  ;; world are (marker flavor . init-plist) lists until
  ;; DBG:BOOTSTRAP-FASD-INSTANCES rebuilds them.  Created lazily.
  (instance-marker 0))

;;; Layout shorthands

(defun cold-dtp (w name)
  "Type code for SYSTEM:DTP-NAME, cached."
  (or (gethash name (cold-world-dtp-cache w))
      (setf (gethash name (cold-world-dtp-cache w))
            (layout-value (cold-world-layout w)
                          (concatenate 'string "SYSTEM:DTP-" name)))))

(defun cold-address (w name)
  "Architectural address constant NAME with its VMA=PMA tag bits stripped."
  (unsigned-vma (layout-value (cold-world-layout w) name)))

;;; Page-level Q access

(defun cw-page (w vma)
  (let ((p (ash vma -8)))
    (or (gethash p (cold-world-pages w))
        (setf (gethash p (cold-world-pages w))
              (make-qvec +ivory-page-size-qs+)))))

(defun cw-touch (w vma)
  "Ensure the page holding VMA exists (present but zero in the emitted world)."
  (cw-page w vma)
  nil)

(defun cw-set (w vma tag data)
  (set-q (cw-page w vma) (logand vma #xFF) tag (ldb (byte 32 0) data)))

(defun cw-ref (w vma)
  "Q at VMA.  Returns (values tag data present-p); absent pages read as 0/0/NIL."
  (let ((qv (gethash (ash vma -8) (cold-world-pages w))))
    (if qv
        (multiple-value-bind (tag data) (qref qv (logand vma #xFF))
          (values tag data t))
        (values 0 0 nil))))

;;; Areas

(defun cold-init-areas (w)
  "Create the cold-load area set from the layout :AREAS section."
  (dolist (entry (layout-section (cold-world-layout w) :areas))
    (destructuring-bind (name number) entry
      (when (< number (length (cold-world-areas w)))
        (setf (aref (cold-world-areas w) number)
              (make-cold-area :number number :name name))))))

(defun cold-area (w designator)
  "Area by number, or by name suffix (\"SYMBOL-AREA\" matches any package)."
  (etypecase designator
    (integer (or (aref (cold-world-areas w) designator)
                 (error "Area ~D not initialized" designator)))
    (string
     (or (loop for a across (cold-world-areas w)
               when (and a (let ((n (cold-area-name a)))
                             (string= designator
                                      (strip-package n))))
                 return a)
         (error "No area named ~A" designator)))))

(defun cold-add-region (w area-designator origin length)
  "Attach a new region [ORIGIN, ORIGIN+LENGTH) to an area."
  (let* ((area (cold-area w area-designator))
         (region (make-cold-region :number (fill-pointer (cold-world-regions w))
                                   :area (cold-area-number area)
                                   :origin origin :length length :free origin)))
    (vector-push-extend region (cold-world-regions w))
    (push (cold-region-number region) (cold-area-regions area))
    region))

(defun cold-area-current-region (w area-designator)
  (let ((area (cold-area w area-designator)))
    (when (cold-area-regions area)
      (aref (cold-world-regions w) (first (cold-area-regions area))))))

(defun cold-alloc (w area-designator nqs)
  "Allocate NQS Qs in AREA's newest region; returns the base VMA.
No automatic region extension yet -- callers add regions explicitly."
  (let ((region (or (cold-area-current-region w area-designator)
                    (error "Area ~A has no region" area-designator))))
    (let ((vma (cold-region-free region)))
      (when (> (+ (- vma (cold-region-origin region)) nqs)
               (cold-region-length region))
        (error "Region ~D (area ~A) full: need ~D Qs at #x~X"
               (cold-region-number region) area-designator nqs vma))
      (setf (cold-region-free region) (+ vma nqs))
      ;; Make the pages the object spans present.
      (loop for page-vma from (logandc2 vma #xFF) to (+ vma nqs -1)
            by +ivory-page-size-qs+
            do (cw-touch w page-vma))
      vma)))

;;; Model assembly: present pages -> :data-pages map entries (contiguous
;;; pages coalesce), header synthesized with the Minima conventions used by
;;; model-from-build-spec (fixnum-0 fill, cookie tags, version Q).

(defun cold-page-runs (w)
  "Sorted list of (start-page . page-count) runs of present pages."
  (let ((pagenos (sort (loop for p being the hash-keys of (cold-world-pages w)
                             collect p)
                       #'<))
        (runs nil))
    (dolist (p pagenos (nreverse runs))
      (if (and runs (= p (+ (caar runs) (cdar runs))))
          (incf (cdar runs))
          (push (cons p 1) runs)))))

(defun cold-world-model (w &key (version-q #x410040) extra-entries)
  "Freeze the cold-world into an :ilod world-model.  EXTRA-ENTRIES are
prebuilt map-entry structures (e.g. :constant entries) appended after the
:data-pages entries."
  (let* ((entries
           (append
            (loop for (start-page . npages) in (cold-page-runs w)
                  collect
                  (let* ((count (* npages +ivory-page-size-qs+))
                         (qv (make-qvec count)))
                    (dotimes (p npages)
                      (let ((src (gethash (+ start-page p) (cold-world-pages w))))
                        (dotimes (i +ivory-page-size-qs+)
                          (multiple-value-bind (tag data) (qref src i)
                            (set-q qv (+ (* p +ivory-page-size-qs+) i) tag data)))))
                    (make-map-entry :address (ash start-page 8)
                                    :opcode +op-data-pages+ :count count
                                    :address-tag +type-fixnum+
                                    :op-tag +type-fixnum+
                                    :data-tag +type-fixnum+
                                    :payload qv)))
            extra-entries))
         (model (make-world-model :format :ilod :wired-map entries))
         (header-pages (assign-ilod-file-pages model))
         (header (make-array (* header-pages +ivory-page-size-bytes+)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (setf (world-model-header-bytes model) header)
    (dotimes (q (* header-pages +ivory-page-size-qs+))
      (ivory-write-q header q +type-fixnum+ 0))
    (ivory-write-q header 0 (aref *ilod-header-tags* 0) version-q)
    (ivory-write-q header 1 (aref *ilod-header-tags* 1) (length entries))
    (ivory-write-q header 2 (aref *ilod-header-tags* 2) 0)
    (ivory-write-q header 3 (aref *ilod-header-tags* 3) 0)
    (synthesize-map-qs model +ivory-first-map-q+)
    model))
