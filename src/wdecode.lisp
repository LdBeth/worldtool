;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Read Lisp objects back out of a loaded world model.
;;;
;;; Used to pull ground truth out of the distribution world: the cold-load
;;; generator's own artifacts (SI:*COLD-LOAD-DEFERRED-FORMS*,
;;; *LINKED-SYMBOL-CELLS*, region tables) survive there as ordinary heap
;;; structure.  Decoding is best-effort: anything beyond the basic object
;;; kinds comes back as an opaque (:Q tag data) marker rather than an error.

(in-package #:worldtool)

;;; Architectural type codes not already in constants.lisp
;;; (i-sys/sysdef.lisp *DATA-TYPES*; cross-checked with cold-layout.sexp).
(defconstant +type-null+             0)
(defconstant +type-evcp+             4)
(defconstant +type-1q-forward+       5)
(defconstant +type-header-forward+   6)
(defconstant +type-element-forward+  7)
(defconstant +type-double-float+    11)
(defconstant +type-bignum+          12)
(defconstant +type-nil-pointer+     20)  ; DTP-NIL (+type-nil+ names it too)
(defconstant +type-list+            21)
(defconstant +type-array+           22)
(defconstant +type-string+          23)
(defconstant +type-symbol+          24)
(defconstant +type-odd-pc+          39)

;;; Decoded-symbol marker: identity is the world VMA.
(defstruct (wsym (:constructor make-wsym (vma name))
                 (:print-function
                  (lambda (s stream depth)
                    (declare (ignore depth))
                    (format stream "~A" (wsym-name s)))))
  vma name)

(defun w-follow-cell (model vma)
  "Follow one-q-forward/EVCP chains; returns (values tag data final-vma)."
  (loop for hops from 0
        do (multiple-value-bind (tag data) (world-q model vma)
             (cond ((null tag) (return (values nil nil vma)))
                   ((and (< hops 16)
                         (member (tag-type tag)
                                 (list +type-1q-forward+ +type-evcp+
                                       +type-element-forward+)))
                    (setf vma data))
                   (t (return (values tag data vma)))))))

(defun w-string (model vma &key (max-length 4096))
  "The character array headed at VMA as a host string, or NIL."
  (multiple-value-bind (tag data) (world-q model vma)
    (when (and tag
               (member (tag-type tag) (list +type-header-i+ +type-header-p+))
               (= (ldb (byte 2 30) data) +array-element-type-character+)
               (not (logbitp 23 data)))         ; short prefix only
      (let ((packing (ldb (byte 3 27) data))
            (len (ldb (byte 15 0) data)))
        (when (and (<= packing 2) (<= len max-length))
          (let* ((per-word (ash 1 packing))
                 (bits (ash 32 (- packing)))
                 (s (make-string len)))
            (dotimes (k len s)
              (multiple-value-bind (dt dd)
                  (world-q model (+ vma 1 (floor k per-word)))
                (declare (ignore dt))
                (unless dd (return nil))
                (let ((c (ldb (byte bits (* bits (mod k per-word))) dd)))
                  (if (< c 256)
                      (setf (char s k) (code-char c))
                      (return nil)))))))))))

(defun w-symbol (model vma)
  "The symbol block at VMA as a wsym (pname decoded), or NIL."
  (multiple-value-bind (tag data) (world-q model vma)
    (when (and tag (= (tag-type tag) +type-header-p+))
      (let ((name (w-string model data)))
        (when name (make-wsym vma name))))))

(defvar *w-decode-limit* 100000
  "Total cells budget per W-DECODE call tree (runaway guard).")

(defun w-decode (model tag data &key (depth 24) (budget (list *w-decode-limit*)))
  "Host representation of the Q TAG:DATA in MODEL.  Conses/symbols/strings/
numbers decode structurally; everything else comes back as (:Q tag data)."
  (cond ((<= depth 0)
         (return-from w-decode (list :depth-cut tag data)))
        ((<= (decf (first budget)) 0)
         (return-from w-decode (list :budget-cut tag data))))
  (let ((type (tag-type tag)))
    (cond
      ((= type +type-fixnum+)
       (if (logbitp 31 data) (- data (ash 1 32)) data))
      ((= type +type-nil-pointer+) nil)
      ((= type +type-symbol+)
       (or (w-symbol model data) (list :q tag data)))
      ((= type +type-string+)
       (or (w-string model data) (list :q tag data)))
      ((= type +type-character+)
       (if (< data 256) (code-char data) (list :char data)))
      ((= type +type-list+) (w-list model data depth budget))
      (t (list :q tag data)))))

(defun w-list (model vma depth budget)
  "Walk the cdr-coded list at VMA."
  (let ((head nil) (tail nil))
    (flet ((emit (x)
             (let ((cell (cons x nil)))
               (if tail (setf (cdr tail) cell) (setf head cell))
               (setf tail cell))))
      (loop
        (when (<= (decf (first budget)) 0)
          (emit (list :length-cut vma))
          (return head))
        (multiple-value-bind (tag data vma*) (w-follow-cell model vma)
          (setf vma vma*)
          (unless tag
            (emit (list :unmapped vma))
            (return head))
          (emit (w-decode model tag data :depth (1- depth) :budget budget))
          (ecase (ash tag -6)
            (0 (incf vma))                       ; cdr-next
            (1 (return head))                    ; cdr-nil
            ((2 3)                               ; cdr-normal
             (multiple-value-bind (ct cd) (w-follow-cell model (1+ vma))
               (cond ((null ct)
                      (setf (cdr tail) (list :unmapped (1+ vma)))
                      (return head))
                     ((= (tag-type ct) +type-list+) (setf vma cd))
                     ((= (tag-type ct) +type-nil-pointer+) (return head))
                     (t (setf (cdr tail)
                              (w-decode model ct cd
                                        :depth (1- depth) :budget budget))
                        (return head)))))))))))

(defgeneric world-find-symbols (model pname)
  (:documentation "All symbol-block VMAs in MODEL whose pname is PNAME
\(case-sensitive).  Scans every data-pages entry -- wired and unwired;
fresh.ilod keeps its heap in the unwired map -- for the 5-Q symbol shape.
Also accepts a refdata/refrec reference oracle (src/refdata.lisp)."))

(defmethod world-find-symbols ((model world-model) pname)
  (let ((hits nil)
        (len (length pname)))
    (dolist (e (append (world-model-wired-map model)
                       (world-model-unwired-map model))
               (nreverse hits))
      (when (= (map-entry-opcode e) +op-data-pages+)
        (let ((qv (map-entry-payload e))
              (base (map-entry-address e)))
          (dotimes (i (map-entry-count e))
            (multiple-value-bind (tag data) (qref qv i)
              ;; Symbol header: dtp-header-p, cdr/header-type 0.
              (when (= tag +type-header-p+)
                (multiple-value-bind (htag hdata) (world-q model data)
                  (when (and htag
                             (member (tag-type htag)
                                     (list +type-header-i+ +type-header-p+))
                             (= (ldb (byte 2 30) hdata)
                                +array-element-type-character+)
                             (not (logbitp 23 hdata))
                             (= (ldb (byte 15 0) hdata) len)
                             (equal (w-string model data) pname))
                    (push (+ base i) hits)))))))))))

;;; ---- Symbol-home oracle -------------------------------------------------
;;;
;;; The original cold-load generator ran inside a full Genera whose package
;;; system resolved every dumped symbol reference; a plain BIN-OP-SYMBOL
;;; means "accessible from the file's package" (dump.lisp DUMP-SYMBOL uses
;;; the printer's prefix logic), which includes inherited symbols.  We
;;; substitute the distribution world: every symbol block in it records its
;;; true home package, so scanning them yields pname -> home-package(s).

(defun w-package-primary-name (model pkg-vma cache)
  "Primary name string of the package object at PKG-VMA.
PKG-NAME-LIST is defstruct slot 0 = leader element 0 (package.lisp:125)."
  (or (gethash pkg-vma cache)
      (setf (gethash pkg-vma cache)
            (multiple-value-bind (tag data) (w-follow-cell model (- pkg-vma 1))
              (and tag
                   (= (tag-type tag) +type-list+)
                   (multiple-value-bind (ct cd) (w-follow-cell model data)
                     (and ct (= (tag-type ct) +type-string+)
                          (w-string model cd))))))))

(defgeneric world-symbol-homes (model)
  (:documentation "(values HOMES ALIASES): HOMES maps pname -> list of home
package primary names; ALIASES maps every package name/nickname -> primary
name.  Also accepts a refdata/refrec reference oracle (src/refdata.lisp)."))

(defmethod world-symbol-homes ((model world-model))
  (let ((homes (make-hash-table :test #'equal))
        (aliases (make-hash-table :test #'equal))
        (pkg-names (make-hash-table)))
    (dolist (e (world-model-wired-map model))
      (when (= (map-entry-opcode e) +op-data-pages+)
        (let ((qv (map-entry-payload e)))
          (dotimes (i (map-entry-count e))
            (multiple-value-bind (tag data) (qref qv i)
              (when (= tag +type-header-p+)     ; symbol header, cdr 0
                (let ((pname (w-string model data)))
                  (when (and pname
                             (< (+ i 4) (map-entry-count e)))
                    (multiple-value-bind (ptag pdata) (qref qv (+ i 4))
                      (when (= (tag-type ptag) +type-array+)
                        (let ((home (w-package-primary-name
                                     model pdata pkg-names)))
                          (when home
                            (pushnew home (gethash pname homes)
                                     :test #'string=)))))))))))))
    ;; Package name/nickname aliases, from each package's name-list.
    (loop for pkg-vma being the hash-keys of pkg-names
          do (multiple-value-bind (tag data)
                 (w-follow-cell model (- pkg-vma 1))
               (when (and tag (= (tag-type tag) +type-list+))
                 (let ((names (ignore-errors (w-decode model tag data))))
                   (when (and (consp names) (stringp (first names)))
                     (dolist (n (rest names))
                       (when (stringp n)
                         (setf (gethash n aliases) (first names)))))))))
    (values homes aliases)))

(defun world-symbol-value (model pname &key (which 0))
  "Decode the value cell of the WHICHth symbol named PNAME.
Returns (values decoded-value symbol-vma n-candidates)."
  (let ((syms (world-find-symbols model pname)))
    (unless syms (error "No symbol named ~A found" pname))
    (let ((vma (nth (min which (1- (length syms))) syms)))
      (multiple-value-bind (tag data) (w-follow-cell model (1+ vma))
        (values (and tag (w-decode model tag data))
                vma (length syms))))))
