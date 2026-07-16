;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; VMA-level access to a world's wired load map, and an annotated dump of
;;; the architectural (vma=pma) region using the layout tables exported from
;;; a running Genera world (worldtool/genera/m3-export-layout.lisp).
;;;
;;; Opcode semantics mirror src/world_tools.c VLMLoadMapData: :constant
;;; replicates the entry's data Q, :constant-incremented increments the data
;;; word per Q, :copy copies COUNT Qs from the VMA in the data word.

(in-package #:worldtool)

(defun find-wired-entry (model vma)
  (loop for e in (append (world-model-wired-map model)
                         (world-model-unwired-map model))
        when (and (<= (map-entry-address e) vma)
                  (< vma (+ (map-entry-address e) (map-entry-count e))))
          return e))

(defgeneric world-q (model vma)
  (:documentation "Read the Q at VMA via the load maps (wired, then
unwired).  Returns (values tag data) or NIL if the address is not mapped.
Also accepts a refdata/refrec reference oracle (src/refdata.lisp)."))

(defmethod world-q ((model world-model) vma)
  (let ((e (find-wired-entry model vma)))
    (when e
      (let ((i (- vma (map-entry-address e))))
        (ecase (map-entry-opcode e)
          (#.+op-data-pages+ (qref (map-entry-payload e) i))
          (#.+op-constant+ (values (map-entry-data-tag e)
                                   (map-entry-data-data e)))
          (#.+op-constant-incremented+
           (values (map-entry-data-tag e)
                   (ldb (byte 32 0) (+ (map-entry-data-data e) i))))
          (#.+op-copy+ (world-q model (+ (map-entry-data-data e) i))))))))

;;; Layout tables (cold-layout.sexp)

(defstruct layout
  (sections nil)     ; alist section-keyword -> entries
  (constants nil)    ; hash "PKG:NAME" -> integer
  (by-name nil))     ; hash "NAME" (no package) -> integer (first definition wins)

(defun strip-package (name)
  (let ((colon (position #\: name)))
    (if colon (subseq name (1+ colon)) name)))

(defun read-layout (path)
  (let ((sections nil)
        (constants (make-hash-table :test #'equal))
        (by-name (make-hash-table :test #'equal)))
    (with-open-file (s path)
      (loop for form = (read s nil nil)
            while form
            do (push (cons (first form) (rest form)) sections)))
    (dolist (section '(:system-constants :sys-integers))
      (dolist (entry (cdr (assoc section sections)))
        (destructuring-bind (name value) entry
          (when (integerp value)
            (unless (nth-value 1 (gethash name constants))
              (setf (gethash name constants) value))
            (unless (nth-value 1 (gethash (strip-package name) by-name))
              (setf (gethash (strip-package name) by-name) value))))))
    (make-layout :sections (nreverse sections)
                 :constants constants
                 :by-name by-name)))

(defun layout-section (layout name)
  (cdr (assoc name (layout-sections layout))))

(defun layout-value (layout name)
  "Look NAME up among the dumped constants; accepts \"PKG:NAME\" or bare \"NAME\"."
  (or (gethash name (layout-constants layout))
      (gethash name (layout-by-name layout))
      (error "~A not in layout dump" name)))

(defun unsigned-vma (value)
  "Strip the VMA=PMA tag bits of a dumped address constant to its low 32."
  (ldb (byte 32 0) value))

;;; Annotated dump

(defun trap-vector-labels (layout)
  "Address -> label map for the trap-vector page, from every dumped constant
named ...-VECTOR whose value fits inside the trap vector region."
  (let ((base (unsigned-vma (layout-value layout "%TRAP-VECTOR-BASE")))
        (length (layout-value layout "%TRAP-VECTOR-LENGTH"))
        (labels (make-hash-table)))
    (maphash (lambda (name value)
               (when (and (> (length name) 7)
                          (string= name "-VECTOR" :start1 (- (length name) 7))
                          (integerp value) (<= 0 value) (< value length))
                 (setf (gethash (+ base value) labels)
                       (strip-package name))))
             (layout-constants layout))
    (values labels base length)))

(defun dump-q-run (stream vma tag data count label)
  (format stream "  #x~8,'0X ~2,'0X:~8,'0X~@[ x~D~]~@[  ~A~]~%"
          vma tag data (when (> count 1) count) label))

(defun dump-vma-range (stream model vma count &key labels (compress t) skip-zeros)
  "Print Qs [VMA, VMA+COUNT) with optional address labels, compressing runs
of identical Qs.  Labeled addresses always break a run."
  (let ((run-vma nil) (run-tag nil) (run-data nil) (run-count 0))
    (flet ((flush ()
             (when run-vma
               (dump-q-run stream run-vma run-tag run-data run-count
                           (and labels (gethash run-vma labels)))
               (setf run-vma nil run-count 0))))
      (dotimes (i count)
        (let ((addr (+ vma i)))
          (multiple-value-bind (tag data) (world-q model addr)
            (cond ((null tag)
                   (flush)
                   (format stream "  #x~8,'0X unwired~%" addr))
                  ((and skip-zeros (zerop tag) (zerop data)
                        (not (and labels (gethash addr labels))))
                   (flush))
                  ((and compress run-vma (= tag run-tag) (= data run-data)
                        (not (and labels (gethash addr labels))))
                   (incf run-count))
                  (t
                   (flush)
                   (setf run-vma addr run-tag tag run-data data run-count 1))))))
      (flush))))

(defun dump-magic-location-block (stream model block)
  (destructuring-bind (name start end ventries) block
    (format stream "~%~A  #x~8,'0X..#x~8,'0X (~D vars):~%"
            name start end (length ventries))
    (loop for (type value) in ventries
          for addr from start
          do (multiple-value-bind (tag data) (world-q model addr)
               (format stream "  #x~8,'0X ~A  ~A ~A~%"
                       addr
                       (if tag
                           (format nil "~2,'0X:~8,'0X" tag data)
                           "--:--------")
                       type
                       (if (consp value) (second value) value))))
    ;; Anything between the last variable and the block's reserved end
    (let ((tail-start (+ start (length ventries))))
      (when (< tail-start end)
        (dump-vma-range stream model tail-start (- end tail-start)
                        :skip-zeros t)))))

(defun inspect-world (world-path layout-path &key vma (stream *standard-output*))
  "Annotated dump of WORLD-PATH's architectural wired state, labeled from
LAYOUT-PATH (cold-layout.sexp).  VMA is an optional (address . count) raw
dump request by virtual address."
  (let ((model (read-world world-path))
        (layout (read-layout layout-path)))
    (cond
      (vma
       (destructuring-bind (address . count) vma
         (format stream "~&Qs at vma #x~8,'0X..#x~8,'0X:~%"
                 address (+ address count -1))
         (dump-vma-range stream model address count :compress nil)))
      (t
       (format stream "~&~A: annotated wired dump (layout: ~A)~%"
               world-path layout-path)
       (let ((total 0))
         (dolist (e (world-model-wired-map model))
           (incf total (map-entry-count e)))
         (format stream "wired map: ~D entries, ~:D Qs~%"
                 (length (world-model-wired-map model)) total))
       ;; Trap vectors
       (multiple-value-bind (labels base length) (trap-vector-labels layout)
         (format stream "~%Trap vectors  #x~8,'0X..#x~8,'0X (zero Qs omitted):~%"
                 base (+ base length -1))
         (dump-vma-range stream model base length :labels labels :skip-zeros t))
       ;; Communication areas and other magic-location blocks
       (dolist (block (layout-section layout :magic-locations))
         (when (find-wired-entry model (second block))
           (dump-magic-location-block stream model block)))
       ;; NIL and T
       (let ((nil-address (unsigned-vma (layout-value layout "NIL-ADDRESS")))
             (t-address (unsigned-vma (layout-value layout "T-ADDRESS"))))
         (format stream "~%NIL structure at #x~8,'0X:~%" nil-address)
         (dump-vma-range stream model nil-address (- t-address nil-address)
                         :compress nil)
         (format stream "T structure at #x~8,'0X:~%" t-address)
         (dump-vma-range stream model t-address 8 :compress nil))
       ;; Occupancy of the rest of the trap-vector map entry
       (let* ((base (unsigned-vma (layout-value layout "%TRAP-VECTOR-BASE")))
              (entry (find-wired-entry model base)))
         (when entry
           (format stream "~%Architectural map entry #x~8,'0X count #x~X: ~
                           nonzero Qs per Ivory page:~%"
                   (map-entry-address entry) (map-entry-count entry))
           (loop with address = (map-entry-address entry)
                 for page from 0 below (ceiling (map-entry-count entry)
                                                +ivory-page-size-qs+)
                 for nonzero = (loop for i from 0 below +ivory-page-size-qs+
                                     count (multiple-value-bind (tag data)
                                               (world-q model (+ address
                                                                 (* page +ivory-page-size-qs+)
                                                                 i))
                                             (and tag (not (and (zerop tag)
                                                                (zerop data))))))
                 when (plusp nonzero)
                   do (format stream "  page #x~8,'0X: ~D~%"
                              (+ address (* page +ivory-page-size-qs+)) nonzero))))))
    model))
