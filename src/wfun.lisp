;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Compiled-function census: find every Ivory compiled function resident
;;; in a world and decode each one's extra-information suffix.
;;;
;;; Block layout (DEFSTORAGE COMPILED-FUNCTION, i-sys/sysdef.lisp; the cold
;;; generator builds exactly this shape in cold-fun.lisp):
;;;   [CCA]               header: dtp-header-i with header type
;;;                       %HEADER-TYPE-COMPILED-FUNCTION (0) in the cdr
;;;                       bits; data word = suffix-size<<18 | total-size
;;;   [CCA+1]             function cell: dtp-compiled-function -> CCA+2
;;;   [CCA+2 ..]          instructions
;;;   [CCA+total-suffix]  extra-info: (<function name> . <debugging-info>)
;;;                       (CCA-EXTRA-INFO, sys2/macro.lisp #+IMACH version)
;;;   [.. CCA+total-1]    rest of the suffix
;;;
;;; This permanently re-implements the one-off scan recorded in the museum
;;; note "Recovering code and assets from a Genera world" (Reproducibility
;;; record): candidates are structural -- header tag, size sanity, and the
;;; function cell pointing back at CCA+2 -- and each candidate's extra-info
;;; is decoded through W-DECODE.  Beyond the name-class counts, the census
;;; reports how many suffix decodes were depth-cut, budget-cut, or contain
;;; opaque/unmapped leftovers: the numbers whose absence downgraded the
;;; original external census to "preliminary".

(in-package #:worldtool)

;;; CCA header data-word fields (DEFSTORAGE COMPILED-FUNCTION,
;;; i-sys/sysdef.lisp: CCA-SUFFIX-SIZE (byte 14 18), CCA-TOTAL-SIZE
;;; (byte 18 0); cold-fun.lisp packs the header the same way).
(defconstant +cca-total-size-position+   0)
(defconstant +cca-total-size-bits+      18)
(defconstant +cca-suffix-size-position+ 18)
(defconstant +cca-suffix-size-bits+     14)

;;; Header tag byte: dtp-header-i, header type
;;; SYSTEM:%HEADER-TYPE-COMPILED-FUNCTION = 0 (cold-layout.sexp) in the
;;; cdr bits, i.e. (tag 0 +type-header-i+).
(defconstant +cca-header-tag+ #x03)

;;; ---- Fast Q lookup ------------------------------------------------------
;;;
;;; The census reads millions of Qs; FIND-WIRED-ENTRY's linear map walk is
;;; too slow for that.  WINDEXED wraps a model with a binary search over an
;;; address-sorted vector of its map entries.  Read-side cache only: the
;;; model's own entry lists are never reordered (their order is load-map
;;; emission order and must be preserved).  Lookups agree with
;;; FIND-WIRED-ENTRY because produced worlds' map entries do not overlap.

(defstruct (windexed (:constructor %make-windexed (model entries)))
  model
  entries)   ; simple-vector of map-entry, ascending by address

(defun windexed (model)
  (%make-windexed
   model
   (sort (coerce (append (world-model-wired-map model)
                         (world-model-unwired-map model))
                 'simple-vector)
         #'< :key #'map-entry-address)))

(defun windexed-entry (v vma)
  "Rightmost entry in the sorted vector V whose range covers VMA, or NIL."
  (let ((lo 0) (hi (length v)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (<= (map-entry-address (svref v mid)) vma)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    (when (plusp lo)
      (let ((e (svref v (1- lo))))
        (when (< (- vma (map-entry-address e)) (map-entry-count e))
          e)))))

(defmethod world-q ((w windexed) vma)
  (let ((e (windexed-entry (windexed-entries w) vma)))
    (when e (map-entry-q w e vma))))

;;; ---- The census ---------------------------------------------------------

(defstruct (cfun (:constructor make-cfun (cca total suffix)))
  cca total suffix
  (name nil) (name-class :failed)
  (depth-cut-p nil) (budget-cut-p nil) (opaque-p nil) (unmapped-p nil))

(defun cfun-fn (c)
  "The function address: what a dtp-compiled-function Q points at."
  (+ (cfun-cca c) 2))

(defun w-marker-kind (x)
  "The marker keyword if X is a W-DECODE cut/opaque marker, else NIL.
Host keywords occur in decoded trees only as markers: Genera symbols
decode to WSYM structs, never to host keywords."
  (and (consp x) (keywordp (car x)) (car x)))

(defun cfun-validate (w cca data)
  "A fresh CFUN if the header Q at CCA (data word DATA) heads a
structurally valid compiled function; NIL otherwise.  The function cell
at CCA+1 is followed through forwards (Genera-8-5's SYSTEM-STARTUP cell
is a 1q-forward into SystemComm) and must be a dtp-compiled-function
pointing back at CCA+2, or a dtp-generic-function (the wired lock
functions at #x88400000 keep the generic object there instead)."
  (let ((total (ldb (byte +cca-total-size-bits+ +cca-total-size-position+)
                    data))
        (suffix (ldb (byte +cca-suffix-size-bits+ +cca-suffix-size-position+)
                     data)))
    (when (and (>= total 3) (<= (+ suffix 2) total))
      (multiple-value-bind (ftag fdata) (w-follow-cell w (1+ cca))
        (when (and ftag
                   (or (and (= (tag-type ftag) +type-compiled-function+)
                            (= fdata (+ cca 2)))
                       (= (tag-type ftag) +type-generic-function+))
                   (nth-value 0 (world-q w (+ cca total -1))))
          (make-cfun cca total suffix))))))

(defun cfun-note-markers (tree rec)
  "Walk the decoded host tree TREE, setting REC's cut/opaque flags for
every W-DECODE marker found."
  (let ((stack (list tree)))
    (loop while stack
          do (let ((x (pop stack)))
               (when (consp x)
                 (case (w-marker-kind x)
                   (:depth-cut (setf (cfun-depth-cut-p rec) t))
                   ((:budget-cut :length-cut)
                    (setf (cfun-budget-cut-p rec) t))
                   ((:q :char) (setf (cfun-opaque-p rec) t))
                   (:unmapped (setf (cfun-unmapped-p rec) t))
                   ((nil)
                    (push (car x) stack)
                    (push (cdr x) stack))))))))

(defun cfun-decode-suffix (w rec depth budget)
  "Decode REC's extra-info Q; record the name, its class, and cut flags."
  (when (plusp (cfun-suffix rec))
    (multiple-value-bind (tag data)
        (w-follow-cell w (+ (cfun-cca rec)
                            (- (cfun-total rec) (cfun-suffix rec))))
      (if (null tag)
          (setf (cfun-unmapped-p rec) t)
          (let ((info (w-decode w tag data :depth depth
                                           :budget (list budget))))
            (cfun-note-markers info rec)
            (when (and (consp info) (not (w-marker-kind info)))
              (let ((name (car info)))
                (setf (cfun-name rec) name
                      (cfun-name-class rec)
                      (cond ((wsym-p name) :symbol)
                            ((not (consp name)) :failed)      ; NIL etc.
                            ((not (w-marker-kind name)) :compound)
                            ;; CLOS methods are named by the method
                            ;; object itself (an instance), which
                            ;; W-DECODE leaves opaque.
                            ((and (eq (w-marker-kind name) :q)
                                  (= (tag-type (second name))
                                     +type-instance+))
                             :instance)
                            (t :failed))))))))))

(defun world-compiled-functions (model &key (depth 24)
                                            (budget *w-decode-limit*))
  "Scan every mapped Q of MODEL for compiled-function headers; returns a
list of CFUN ascending by CCA.  DEPTH and BUDGET bound each candidate's
extra-info decode (the budget is per function)."
  (let ((w (windexed model))
        (recs nil))
    (dolist (e (append (world-model-wired-map model)
                       (world-model-unwired-map model)))
      (when (= (map-entry-opcode e) +op-data-pages+)
        (let ((qv (map-entry-payload e))
              (base (map-entry-address e)))
          (dotimes (i (map-entry-count e))
            (multiple-value-bind (tag data) (qref qv i)
              (when (= tag +cca-header-tag+)
                (let ((rec (cfun-validate w (+ base i) data)))
                  (when rec
                    (cfun-decode-suffix w rec depth budget)
                    (push rec recs)))))))))
    (sort (nreverse recs) #'< :key #'cfun-cca)))

(defun cfun-clean-p (r)
  (not (or (cfun-depth-cut-p r) (cfun-budget-cut-p r)
           (cfun-opaque-p r) (cfun-unmapped-p r))))

(defun write-functions-listing (out path recs depth budget)
  (with-open-file (s out :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
    (format s ";;; compiled-function census: ~A~%" path)
    (format s ";;; suffix decode depth ~D, budget ~D~%" depth budget)
    (format s ";;; FN-VMA (= CCA+2)  flags (D depth-cut, B budget-cut, ~
O opaque, U unmapped)  name~%")
    (let ((*print-length* 32) (*print-level* 8) (*print-pretty* nil))
      (dolist (r recs)
        (format s "#x~8,'0X ~C~C~C~C ~S~%"
                (cfun-fn r)
                (if (cfun-depth-cut-p r) #\D #\-)
                (if (cfun-budget-cut-p r) #\B #\-)
                (if (cfun-opaque-p r) #\O #\-)
                (if (cfun-unmapped-p r) #\U #\-)
                (cfun-name r))))))

(defun functions-world (path &key (depth 24) (budget *w-decode-limit*)
                                  listing (stream *standard-output*))
  "The `functions` subcommand: census of compiled functions in PATH."
  (let* ((model (read-world path))
         (recs (world-compiled-functions model :depth depth :budget budget)))
    (format stream "~&~A: ~:D compiled-function candidates ~
\(suffix decode depth ~D, budget ~:D)~%"
            path (length recs) depth budget)
    (format stream "  names: ~:D simple symbols, ~:D compound function ~
specs, ~:D instance-named (method objects), ~:D nil/failed~%"
            (count :symbol recs :key #'cfun-name-class)
            (count :compound recs :key #'cfun-name-class)
            (count :instance recs :key #'cfun-name-class)
            (count :failed recs :key #'cfun-name-class))
    (format stream "  suffix decodes: ~:D clean, ~:D depth-cut, ~
~:D budget-cut, ~:D with opaque objects, ~:D with unmapped Qs~%"
            (count-if #'cfun-clean-p recs)
            (count-if #'cfun-depth-cut-p recs)
            (count-if #'cfun-budget-cut-p recs)
            (count-if #'cfun-opaque-p recs)
            (count-if #'cfun-unmapped-p recs))
    (when listing
      (write-functions-listing listing path recs depth budget)
      (format stream "  listing: ~A~%" listing))
    recs))
