;;; -*- Mode: Lisp; Package: WORLDTOOL -*-
;;; Ivory macroinstruction disassembler over world models.
;;;
;;; Instruction encoding (i-sys/sysdef.lisp DEFSYSBYTEs; opcode numbers from
;;; i-sys/opdef.lisp, printing conventions from i-compiler/disassemble.lisp):
;;;
;;;   A code word is a 40-bit Q: 32 data bits + 8 tag bits.  Tag types
;;;   #o60..#o77 (48..63) are DTP-PACKED-INSTRUCTION-*: two 18-bit halfword
;;;   instructions -- even = data bits 0..17; odd = data bits 18..31 (low 14)
;;;   + tag type low 4 bits (high 4).  The tag's cdr field is the sequencing
;;;   code: 0 PC+1, 1 fence (end of instructions), 2 PC-1, 3 even+2/odd+3
;;;   (full-word instructions).  Tag types #o50..#o57 are full-word call
;;;   instructions; every other type is a full-word PUSH-CONSTANT (EVCP =
;;;   PUSH-INDIRECT).
;;;
;;;   A halfword is opcode (bits 10..17) + operand (bits 0..9).  Most opcodes
;;;   take a stack-address operand: mode bits 8..9 (0 FP|n, 1 LP|n, 2 SP:
;;;   operand 0 = pop, else SP|operand-255, 3 immediate, sign-extended for
;;;   opcodes whose opdef operand types include SIGNED) over an 8-bit offset.
;;;
;;;   Relative PCs count halfwords from CCA+2 (word = CCA + 2 + pc/2), the
;;;   same numbering Genera's own DISASSEMBLE prints (we print them in
;;;   decimal, not octal).

(in-package #:worldtool)

;;; ---- Architectural constants -------------------------------------------

(defconstant +type-call-compiled-even+          40)
(defconstant +type-call-compiled-odd+           41)
(defconstant +type-call-indirect+               42)
(defconstant +type-call-generic+                43)
(defconstant +type-call-compiled-even-prefetch+ 44)
(defconstant +type-call-compiled-odd-prefetch+  45)
(defconstant +type-call-indirect-prefetch+      46)
(defconstant +type-call-generic-prefetch+       47)
(defconstant +type-packed-instruction-low+      48)

;;; #o41 = DTP-SPARE-IMMEDIATE-1, reused by the VLM as the native
;;; (translated Alpha) instruction type (opdef.lisp
;;; *VLM-NATIVE-INSTRUCTION-OPCODE*).
(defconstant +type-native-instruction+          33)

(defparameter *data-type-names*
  ;; Tag type 0..63, i-sys/opdef.lisp DEF-DTP-OP order.
  #("NULL" "MONITOR-FORWARD" "HEADER-P" "HEADER-I" "EVCP" "ONE-Q-FORWARD"
    "HEADER-FORWARD" "ELEMENT-FORWARD" "FIXNUM" "SMALL-RATIO" "SINGLE-FLOAT"
    "DOUBLE-FLOAT" "BIGNUM" "BIG-RATIO" "COMPLEX" "SPARE-NUMBER" "INSTANCE"
    "LIST-INSTANCE" "ARRAY-INSTANCE" "STRING-INSTANCE" "NIL" "LIST" "ARRAY"
    "STRING" "SYMBOL" "LOCATIVE" "LEXICAL-CLOSURE" "DYNAMIC-CLOSURE"
    "COMPILED-FUNCTION" "GENERIC-FUNCTION" "SPARE-POINTER-1" "SPARE-POINTER-2"
    "PHYSICAL-ADDRESS" "SPARE-IMMEDIATE-1" "BOUND-LOCATION" "CHARACTER"
    "LOGIC-VARIABLE" "GC-FORWARD" "EVEN-PC" "ODD-PC" "CALL-COMPILED-EVEN"
    "CALL-COMPILED-ODD" "CALL-INDIRECT" "CALL-GENERIC"
    "CALL-COMPILED-EVEN-PREFETCH" "CALL-COMPILED-ODD-PREFETCH"
    "CALL-INDIRECT-PREFETCH" "CALL-GENERIC-PREFETCH"
    "PACKED-INSTRUCTION-60" "PACKED-INSTRUCTION-61" "PACKED-INSTRUCTION-62"
    "PACKED-INSTRUCTION-63" "PACKED-INSTRUCTION-64" "PACKED-INSTRUCTION-65"
    "PACKED-INSTRUCTION-66" "PACKED-INSTRUCTION-67" "PACKED-INSTRUCTION-70"
    "PACKED-INSTRUCTION-71" "PACKED-INSTRUCTION-72" "PACKED-INSTRUCTION-73"
    "PACKED-INSTRUCTION-74" "PACKED-INSTRUCTION-75" "PACKED-INSTRUCTION-76"
    "PACKED-INSTRUCTION-77"))

(defparameter *value-dispositions* #("EFFECT" "VALUE" "RETURN" "MULTIPLE"))

(defparameter *memory-cycle-types*
  ;; i-sys/sysdef.lisp *MEMORY-CYCLE-TYPES*, sans the %MEMORY- prefix.
  #("DATA-READ" "DATA-WRITE" "BIND-READ" "BIND-WRITE" "BIND-READ-NO-MONITOR"
    "BIND-WRITE-NO-MONITOR" "HEADER" "STRUCTURE-OFFSET" "SCAVENGE" "CDR"
    "GC-COPY" "RAW" "RAW-TRANSLATE"))

(defparameter *internal-registers*
  ;; emulator/ivory.h InternalRegisters (octal), incl. the constants
  ;; RETURN-SINGLE distinguishes.
  '((#o0 . "EA") (#o1 . "FP") (#o2 . "LP") (#o3 . "SP") (#o4 . "MACRO-SP")
    (#o5 . "STACK-CACHE-LOWER-BOUND") (#o6 . "BAR-0") (#o206 . "BAR-1")
    (#o406 . "BAR-2") (#o606 . "BAR-3") (#o7 . "PHT-HASH-0")
    (#o207 . "PHT-HASH-1") (#o407 . "PHT-HASH-2") (#o607 . "PHT-HASH-3")
    (#o10 . "EPC") (#o11 . "DPC") (#o12 . "CONTINUATION")
    (#o13 . "ALU-AND-ROTATE-CONTROL") (#o14 . "CONTROL-REGISTER")
    (#o15 . "CR-ARGUMENT-SIZE") (#o16 . "EPHEMERAL-OLDSPACE-REGISTER")
    (#o17 . "ZONE-OLDSPACE-REGISTER") (#o20 . "CHIP-REVISION")
    (#o21 . "FP-COPROCESSOR-PRESENT") (#o23 . "PREEMPT-REGISTER")
    (#o24 . "ICACHE-CONTROL") (#o25 . "PREFETCHER-CONTROL")
    (#o26 . "MAP-CACHE-CONTROL") (#o27 . "MEMORY-CONTROL")
    (#o30 . "ECC-LOG") (#o31 . "ECC-LOG-ADDRESS")
    (#o32 . "INVALIDATE-MAP-0") (#o232 . "INVALIDATE-MAP-1")
    (#o432 . "INVALIDATE-MAP-2") (#o632 . "INVALIDATE-MAP-3")
    (#o33 . "LOAD-MAP-0") (#o233 . "LOAD-MAP-1") (#o433 . "LOAD-MAP-2")
    (#o633 . "LOAD-MAP-3") (#o34 . "STACK-CACHE-OVERFLOW-LIMIT")
    (#o35 . "UCODE-ROM-CONTENTS") (#o37 . "ADDRESS-MASK")
    (#o40 . "ENTRY-MAXIMUM-ARGUMENTS") (#o41 . "LEXICAL-VARIABLE")
    (#o42 . "INSTRUCTION") (#o44 . "MEMORY-DATA") (#o45 . "DATA-PINS")
    (#o46 . "EXTENSION-REGISTER") (#o47 . "MICROSECOND-CLOCK")
    (#o50 . "ARRAY-HEADER-LENGTH") (#o52 . "LOAD-BAR-0")
    (#o252 . "LOAD-BAR-1") (#o452 . "LOAD-BAR-2") (#o652 . "LOAD-BAR-3")
    (#o1000 . "TOS") (#o1001 . "EVENT-COUNT")
    (#o1002 . "BINDING-STACK-POINTER") (#o1003 . "CATCH-BLOCK-LIST")
    (#o1004 . "CONTROL-STACK-LIMIT") (#o1005 . "CONTROL-STACK-EXTRA-LIMIT")
    (#o1006 . "BINDING-STACK-LIMIT") (#o1007 . "PHT-BASE")
    (#o1010 . "PHT-MASK") (#o1011 . "COUNT-MAP-RELOADS")
    (#o1012 . "LIST-CACHE-AREA") (#o1013 . "LIST-CACHE-ADDRESS")
    (#o1014 . "LIST-CACHE-LENGTH") (#o1015 . "STRUCTURE-CACHE-AREA")
    (#o1016 . "STRUCTURE-CACHE-ADDRESS") (#o1017 . "STRUCTURE-CACHE-LENGTH")
    (#o1020 . "DYNAMIC-BINDING-CACHE-BASE")
    (#o1021 . "DYNAMIC-BINDING-CACHE-MASK") (#o1022 . "CHOICE-POINTER")
    (#o1023 . "STRUCTURE-STACK-CHOICE-POINTER")
    (#o1024 . "FEP-MODE-TRAP-VECTOR-ADDRESS") (#o1026 . "MAPPING-TABLE-CACHE")
    (#o1027 . "MAPPING-TABLE-LENGTH") (#o1030 . "STACK-FRAME-MAXIMUM-SIZE")
    (#o1031 . "STACK-CACHE-DUMP-QUANTUM") (#o1040 . "CONSTANT-NIL")
    (#o1041 . "CONSTANT-T")))

;;; ---- Opcode table -------------------------------------------------------
;;;
;;; (opcode-or-list name format . flags), opcodes in octal exactly as in
;;; i-sys/opdef.lisp.  Formats:
;;;   :none        no operand printed
;;;   :addr        stack-address operand (:signed flag = signed immediates)
;;;   :addr-1      :addr but operand biased +1 (FAST-AREF-1 array register)
;;;   :imm10       10-bit unsigned immediate
;;;   :branch      10-bit signed halfword offset (operand 0 in the
;;;                branch-true/false group = the TRAP-IF encodings)
;;;   :byte-ldb    (Byte size (32-rotation)&31); the LDB size-32 form is ROT
;;;   :byte-dpb    (Byte size rotation)
;;;   :type-member 12-bit operand: N bits 8..11 (incl. the 2 low opcode
;;;                bits), background mask bits 0..7; tests types 4N+i mod 64
;;;   :entry       whole-word entry instruction (min/max args)
;;;   :lexical     N = opcode low 3 bits, plus a stack-address operand
;;;   :finish-call, :return-single, :catch-open, :unbind, :block, :register
;;;                the special printers from i-compiler/disassemble.lisp

(defparameter *packed-op-specs*
  '((#o0 "CAR" :addr) (#o1 "CDR" :addr)
    (#o140 "SET-TO-CAR" :addr) (#o141 "SET-TO-CDR" :addr)
    (#o142 "SET-TO-CDR-PUSH-CAR" :addr)
    (#o200 "RPLACA" :addr :signed) (#o201 "RPLACD" :addr :signed)
    (#o225 "RGETF" :addr) (#o226 "MEMBER" :addr) (#o227 "ASSOC" :addr)
    (#o13 "DEREFERENCE" :addr :signed) (#o237 "UNIFY" :addr :signed)
    (#o103 "PUSH-LOCAL-LOGIC-VARIABLES" :addr)
    (#o55 "PUSH-GLOBAL-LOGIC-VARIABLE" :none)
    (#o14 "LOGIC-TAIL-TEST" :addr :signed)
    (#o270 "EQ" :addr :signed) (#o274 "EQ-NO-POP" :addr :signed)
    (#o263 "EQL" :addr :signed) (#o267 "EQL-NO-POP" :addr :signed)
    (#o260 "EQUAL-NUMBER" :addr :signed)
    (#o264 "EQUAL-NUMBER-NO-POP" :addr :signed)
    (#o262 "GREATERP" :addr :signed) (#o266 "GREATERP-NO-POP" :addr :signed)
    (#o261 "LESSP" :addr :signed) (#o265 "LESSP-NO-POP" :addr :signed)
    (#o273 "LOGTEST" :addr :signed) (#o277 "LOGTEST-NO-POP" :addr :signed)
    ((#o40 #o41 #o42 #o43) "TYPE-MEMBER" :type-member)
    ((#o44 #o45 #o46 #o47) "TYPE-MEMBER-NO-POP" :type-member)
    (#o2 "ENDP" :addr :signed) (#o36 "PLUSP" :addr :signed)
    (#o35 "MINUSP" :addr :signed) (#o34 "ZEROP" :addr :signed)
    (#o300 "ADD" :addr) (#o301 "SUB" :addr) (#o114 "UNARY-MINUS" :addr)
    (#o143 "INCREMENT" :addr) (#o144 "DECREMENT" :addr)
    (#o202 "MULTIPLY" :addr :signed) (#o203 "QUOTIENT" :addr :signed)
    (#o204 "CEILING" :addr :signed) (#o205 "FLOOR" :addr :signed)
    (#o206 "TRUNCATE" :addr :signed) (#o207 "ROUND" :addr :signed)
    (#o211 "RATIONAL-QUOTIENT" :addr :signed)
    (#o213 "MAX" :addr :signed) (#o212 "MIN" :addr :signed)
    (#o215 "LOGAND" :addr :signed) (#o217 "LOGIOR" :addr :signed)
    (#o216 "LOGXOR" :addr :signed)
    (#o232 "ASH" :addr :signed) (#o220 "ROT" :addr :signed)
    (#o221 "LSH" :addr :signed)
    (#o302 "%32-BIT-PLUS" :addr) (#o303 "%32-BIT-DIFFERENCE" :addr)
    (#o222 "%MULTIPLY-DOUBLE" :addr :signed)
    (#o304 "%ADD-BIGNUM-STEP" :addr) (#o305 "%SUB-BIGNUM-STEP" :addr)
    (#o306 "%MULTIPLY-BIGNUM-STEP" :addr) (#o307 "%DIVIDE-BIGNUM-STEP" :addr)
    (#o223 "%LSHC-BIGNUM-STEP" :addr :signed)
    (#o16 "%DOUBLE-FLOAT-OP" :addr)
    (#o100 "PUSH" :addr) (#o340 "POP" :addr) (#o341 "MOVEM" :addr)
    (#o101 "PUSH-N-NILS" :addr)
    (#o150 "PUSH-ADDRESS" :addr) (#o151 "SET-SP-TO-ADDRESS" :addr)
    (#o152 "SET-SP-TO-ADDRESS-SAVE-TOS" :addr)
    (#o102 "PUSH-ADDRESS-SP-RELATIVE" :addr)
    (#o224 "STACK-BLT" :addr :signed) (#o352 "STACK-BLT-ADDRESS" :addr)
    (#o170 "LDB" :byte-ldb) (#o370 "DPB" :byte-dpb)
    (#o171 "CHAR-LDB" :byte-ldb) (#o371 "CHAR-DPB" :byte-dpb)
    (#o172 "%P-LDB" :byte-ldb) (#o372 "%P-DPB" :byte-dpb)
    (#o173 "%P-TAG-LDB" :byte-ldb) (#o373 "%P-TAG-DPB" :byte-dpb)
    (#o312 "AREF-1" :addr) (#o310 "ASET-1" :addr) (#o313 "ALOC-1" :addr)
    (#o3 "SETUP-1D-ARRAY" :addr :signed)
    (#o4 "SETUP-FORCE-1D-ARRAY" :addr :signed)
    (#o350 "FAST-AREF-1" :addr-1) (#o351 "FAST-ASET-1" :addr-1)
    (#o316 "ARRAY-LEADER" :addr) (#o314 "STORE-ARRAY-LEADER" :addr)
    (#o317 "ALOC-LEADER" :addr)
    (#o174 "BRANCH" :branch)
    (#o60 "BRANCH-TRUE" :branch)
    (#o61 "BRANCH-TRUE-ELSE-EXTRA-POP" :branch)
    (#o62 "BRANCH-TRUE-AND-EXTRA-POP" :branch)
    (#o63 "BRANCH-TRUE-EXTRA-POP" :branch)
    (#o64 "BRANCH-TRUE-NO-POP" :branch)
    (#o65 "BRANCH-TRUE-AND-NO-POP" :branch)
    (#o66 "BRANCH-TRUE-ELSE-NO-POP" :branch)
    (#o67 "BRANCH-TRUE-AND-NO-POP-ELSE-NO-POP-EXTRA-POP" :branch)
    (#o70 "BRANCH-FALSE" :branch)
    (#o71 "BRANCH-FALSE-ELSE-EXTRA-POP" :branch)
    (#o72 "BRANCH-FALSE-AND-EXTRA-POP" :branch)
    (#o73 "BRANCH-FALSE-EXTRA-POP" :branch)
    (#o74 "BRANCH-FALSE-NO-POP" :branch)
    (#o75 "BRANCH-FALSE-AND-NO-POP" :branch)
    (#o76 "BRANCH-FALSE-ELSE-NO-POP" :branch)
    (#o77 "BRANCH-FALSE-AND-NO-POP-ELSE-NO-POP-EXTRA-POP" :branch)
    (#o175 "LOOP-DECREMENT-TOS" :branch)
    (#o375 "LOOP-INCREMENT-TOS-LESS-THAN" :branch)
    (#o120 "%BLOCK-0-READ" :block) (#o121 "%BLOCK-1-READ" :block)
    (#o122 "%BLOCK-2-READ" :block) (#o123 "%BLOCK-3-READ" :block)
    (#o124 "%BLOCK-0-READ-SHIFT" :block) (#o125 "%BLOCK-1-READ-SHIFT" :block)
    (#o126 "%BLOCK-2-READ-SHIFT" :block) (#o127 "%BLOCK-3-READ-SHIFT" :block)
    (#o160 "%BLOCK-0-READ-ALU" :addr) (#o161 "%BLOCK-1-READ-ALU" :addr)
    (#o162 "%BLOCK-2-READ-ALU" :addr) (#o163 "%BLOCK-3-READ-ALU" :addr)
    (#o130 "%BLOCK-0-READ-TEST" :block) (#o131 "%BLOCK-1-READ-TEST" :block)
    (#o132 "%BLOCK-2-READ-TEST" :block) (#o133 "%BLOCK-3-READ-TEST" :block)
    (#o30 "%BLOCK-0-WRITE" :addr :signed) (#o31 "%BLOCK-1-WRITE" :addr :signed)
    (#o32 "%BLOCK-2-WRITE" :addr :signed) (#o33 "%BLOCK-3-WRITE" :addr :signed)
    (#o10 "START-CALL" :addr :signed)
    (#o134 "FINISH-CALL-N" :finish-call)
    (#o135 "FINISH-CALL-N-APPLY" :finish-call)
    (#o136 "FINISH-CALL-TOS" :finish-call)
    (#o137 "FINISH-CALL-TOS-APPLY" :finish-call)
    (#o176 "ENTRY-REST-ACCEPTED" :entry)
    (#o177 "ENTRY-REST-NOT-ACCEPTED" :entry)
    (#o50 "LOCATE-LOCALS" :none)
    (#o115 "RETURN-SINGLE" :return-single)
    (#o104 "RETURN-MULTIPLE" :addr) (#o105 "RETURN-KLUDGE" :addr)
    (#o106 "TAKE-VALUES" :addr)
    (#o236 "BIND-LOCATIVE-TO-VALUE" :addr :signed)
    (#o5 "BIND-LOCATIVE" :addr)
    (#o107 "UNBIND-N" :unbind)
    (#o6 "%RESTORE-BINDING-STACK" :addr)
    (#o376 "CATCH-OPEN" :catch-open) (#o51 "CATCH-CLOSE" :none)
    ((#o20 #o21 #o22 #o23 #o24 #o25 #o26 #o27) "PUSH-LEXICAL-VAR" :lexical)
    ((#o240 #o241 #o242 #o243 #o244 #o245 #o246 #o247)
     "POP-LEXICAL-VAR" :lexical)
    ((#o250 #o251 #o252 #o253 #o254 #o255 #o256 #o257)
     "MOVEM-LEXICAL-VAR" :lexical)
    (#o110 "PUSH-INSTANCE-VARIABLE" :addr)
    (#o320 "POP-INSTANCE-VARIABLE" :addr)
    (#o321 "MOVEM-INSTANCE-VARIABLE" :addr)
    (#o111 "PUSH-ADDRESS-INSTANCE-VARIABLE" :addr)
    (#o112 "PUSH-INSTANCE-VARIABLE-ORDERED" :addr)
    (#o322 "POP-INSTANCE-VARIABLE-ORDERED" :addr)
    (#o323 "MOVEM-INSTANCE-VARIABLE-ORDERED" :addr)
    (#o113 "PUSH-ADDRESS-INSTANCE-VARIABLE-ORDERED" :addr)
    (#o324 "%INSTANCE-REF" :addr) (#o325 "%INSTANCE-SET" :addr)
    (#o326 "%INSTANCE-LOC" :addr)
    (#o7 "%EPHEMERALP" :addr)
    (#o331 "%UNSIGNED-LESSP" :addr) (#o335 "%UNSIGNED-LESSP-NO-POP" :addr)
    (#o214 "%ALU" :addr :signed)
    (#o311 "%ALLOCATE-LIST-BLOCK" :addr)
    (#o315 "%ALLOCATE-STRUCTURE-BLOCK" :addr)
    (#o230 "%POINTER-PLUS" :addr :signed)
    (#o231 "%POINTER-DIFFERENCE" :addr :signed)
    (#o145 "%POINTER-INCREMENT" :addr)
    (#o154 "%READ-INTERNAL-REGISTER" :register)
    (#o155 "%WRITE-INTERNAL-REGISTER" :register)
    (#o156 "%COPROCESSOR-READ" :register)
    (#o157 "%COPROCESSOR-WRITE" :register)
    (#o116 "%MEMORY-READ" :block) (#o117 "%MEMORY-READ-ADDRESS" :block)
    (#o12 "%TAG" :addr :signed) (#o327 "%SET-TAG" :addr)
    (#o233 "STORE-CONDITIONAL" :addr :signed)
    (#o234 "%MEMORY-WRITE" :addr :signed)
    (#o235 "%P-STORE-CONTENTS" :addr :signed)
    (#o146 "%SET-CDR-CODE-1" :addr) (#o147 "%SET-CDR-CODE-2" :addr)
    (#o342 "%MERGE-CDR-NO-POP" :addr)
    (#o52 "%GENERIC-DISPATCH" :none) (#o53 "%MESSAGE-DISPATCH" :none)
    (#o11 "%JUMP" :addr)
    (#o54 "%CHECK-PREEMPT-REQUEST" :none)
    (#o56 "NO-OP" :none) (#o57 "%HALT" :none)
    (#o15 "%PROC-BREAKPOINT" :none)
    (#o377 "%HACK" :imm10)))

(defparameter *packed-ops*
  (let ((v (make-array 256 :initial-element nil)))
    (dolist (spec *packed-op-specs* v)
      (destructuring-bind (opcodes name format &rest flags) spec
        (dolist (op (if (listp opcodes) opcodes (list opcodes)))
          (when (aref v op)
            (error "duplicate opcode #o~O: ~A vs ~A"
                   op name (second (aref v op))))
          (setf (aref v op) (list* name format flags)))))))

;;; ---- Field extraction ---------------------------------------------------

(defun sign-extend (value bits)
  (if (logbitp (1- bits) value) (- value (ash 1 bits)) value))

(defun q-even-instruction (data) (ldb (byte 18 0) data))

(defun q-odd-instruction (tag data)
  (logior (ash (ldb (byte 4 0) tag) 14) (ldb (byte 14 18) data)))

(defun insn-opcode (bits) (ldb (byte 8 10) bits))
(defun insn-operand10 (bits) (ldb (byte 10 0) bits))
(defun insn-operand8 (bits) (ldb (byte 8 0) bits))
(defun insn-mode (bits) (ldb (byte 2 8) bits))

(defun format-address-operand (bits signed)
  "The stack-address operand of BITS, as Genera prints it."
  (let ((operand (insn-operand8 bits)))
    (ecase (insn-mode bits)
      (0 (format nil "FP|~D" operand))
      (1 (format nil "LP|~D" operand))
      (2 (if (zerop operand)
             "SP|POP"
             (format nil "SP|~D" (- operand 255))))
      (3 (format nil "~D" (if signed (sign-extend operand 8) operand))))))

;;; ---- Operand-object rendering ------------------------------------------

(defun render-decoded (x)
  "Compact one-line print of a W-DECODE result (markers included)."
  (with-output-to-string (s)
    (labels ((emit (x)
               (cond ((wsym-p x) (write-string (wsym-name x) s))
                     ((stringp x) (prin1 x s))
                     ((null x) (write-string "NIL" s))
                     ((characterp x) (prin1 x s))
                     ((integerp x) (format s "~D" x))
                     ((consp x)
                      (case (w-marker-kind x)
                        (:q (format s "#<Q ~2,'0X:~8,'0X>"
                                    (second x) (third x)))
                        (:char (format s "#<CHAR ~D>" (second x)))
                        (:unmapped (format s "#<UNMAPPED #x~8,'0X>" (second x)))
                        ((:depth-cut :budget-cut :length-cut)
                         (write-string "..." s))
                        (t (write-char #\( s)
                           (loop for head = x then (cdr head)
                                 while (consp head)
                                 do (emit (car head))
                                    (cond ((null (cdr head)))
                                          ((and (consp (cdr head))
                                                (not (w-marker-kind (cdr head))))
                                           (write-char #\Space s))
                                          (t (write-string " . " s)
                                             (emit (cdr head))
                                             (return))))
                           (write-char #\) s))))
                     (t (format s "~S" x)))))
      (emit x))))

(defun render-q (w tag data &optional fnmap)
  ;; Locative constants are almost always symbol-cell pointers (the
  ;; BIND-LOCATIVE / RPLACD-on-value-cell idioms); render them like
  ;; Genera's @SYMBOL rather than as opaque Qs.
  (if (= (tag-type tag) +type-locative+)
      (format nil "@~A" (render-locative w data fnmap))
      (render-decoded (w-decode w tag data :depth 4 :budget (list 64)))))

(defun decode-symbol-cell (w vma)
  "If VMA is a cell of a symbol block, (values wsym cell-index 0..4)."
  (dotimes (k 5)
    (let ((sym (w-symbol w (- vma k))))
      (when sym (return (values sym k))))))

(defun render-locative (w vma &optional fnmap)
  "Render the locative operand of PUSH-INDIRECT / START-CALL-INDIRECT.
When the symbol block is undecodable (Minima worlds leave pnames at
unshipped build-time addresses) but FNMAP is given, follow the cell and
name the compiled function it holds."
  (multiple-value-bind (sym cell) (decode-symbol-cell w vma)
    (cond
      (sym
       (case cell
         (0 (format nil "'~A" (wsym-name sym)))
         (1 (wsym-name sym))                     ; value cell
         (2 (format nil "#'~A" (wsym-name sym))) ; function cell
         (3 (format nil "~A(plist)" (wsym-name sym)))
         (4 (format nil "~A(package)" (wsym-name sym)))))
      (fnmap
       (multiple-value-bind (tag data) (w-follow-cell w vma)
         (let ((rec (and tag
                         (member (tag-type tag)
                                 (list +type-compiled-function+
                                       +type-even-pc+ +type-odd-pc+))
                         (fnmap-containing fnmap data))))
           (cond ((null rec) (format nil "@#x~8,'0X" vma))
                 ;; A real (symbol-named or donor-named) target reads
                 ;; better by name; marker-named ones (Minima) by their
                 ;; listing address.
                 ((or (fnmap-donor-name fnmap (cfun-fn rec))
                      (member (cfun-name-class rec) (list :symbol :compound)))
                  (format nil "@#x~8,'0X -> FN-#x~8,'0X ~A"
                          vma (cfun-fn rec) (render-fn-name rec fnmap)))
                 (t (format nil "@#x~8,'0X -> FN-#x~8,'0X"
                            vma (cfun-fn rec)))))))
      (t (format nil "@#x~8,'0X" vma)))))

;;; ---- Function map (for call-target resolution) --------------------------

(defstruct (fnmap (:constructor %make-fnmap (ccas recs names)))
  ccas    ; simple-vector of cca, ascending
  recs    ; simple-vector of cfun, same order
  names)  ; hash fn-vma -> donor name string, or NIL

(defun make-fnmap (recs &optional names)
  (let ((v (coerce recs 'simple-vector)))
    (%make-fnmap (map 'simple-vector #'cfun-cca v) v names)))

(defun read-names-file (path)
  "Donor-name map: one `#xVMA NAME` pair per line, `;` starts a comment.
Returns a hash of vma -> name."
  (let ((names (make-hash-table)))
    (with-open-file (s path)
      (loop for line = (read-line s nil)
            while line
            do (let* ((text (subseq line 0 (position #\; line)))
                      (fields (let ((acc nil) (start 0))
                                (loop
                                  (let ((pos (position #\Space text
                                                       :start start
                                                       :test-not #'eql)))
                                    (unless pos (return (nreverse acc)))
                                    (let ((end (or (position #\Space text
                                                             :start pos)
                                                   (length text))))
                                      (push (subseq text pos end) acc)
                                      (setf start end)))))))
                 (when fields
                   (unless (= (length fields) 2)
                     (error "names file ~A: malformed line ~S" path line))
                   (let ((addr (first fields)))
                     (setf (gethash (parse-integer
                                     addr
                                     :start (if (and (> (length addr) 2)
                                                     (string-equal "#x" addr
                                                                   :end2 2))
                                                2 0)
                                     :radix 16)
                                    names)
                           (second fields)))))))
    names))

(defun fnmap-donor-name (m vma)
  (let ((names (and m (fnmap-names m))))
    (and names (gethash vma names))))

(defun fnmap-containing (m vma)
  "The cfun whose block [cca, cca+total) contains VMA, or NIL."
  (let* ((ccas (fnmap-ccas m))
         (lo 0) (hi (length ccas)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (<= (svref ccas mid) vma)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    (when (plusp lo)
      (let ((r (svref (fnmap-recs m) (1- lo))))
        (when (< (- vma (cfun-cca r)) (cfun-total r))
          r)))))

(defun render-fn-name (rec &optional fnmap)
  (or (and fnmap (fnmap-donor-name fnmap (cfun-fn rec)))
      (if (cfun-name rec)
          (render-decoded (cfun-name rec))
          (format nil "FN-#x~8,'0X" (cfun-fn rec)))))

(defun render-call-target (fnmap word-vma odd)
  (let ((rec (and fnmap (fnmap-containing fnmap word-vma))))
    (cond ((null rec)
           (format nil "#x~8,'0X~:[~;.ODD~]" word-vma odd))
          ((and (= word-vma (cfun-fn rec)) (not odd))
           (render-fn-name rec fnmap))
          (t (format nil "~A+~D~:[~;.ODD~]"
                     (render-fn-name rec fnmap) (- word-vma (cfun-fn rec))
                     odd)))))

;;; ---- Packed-halfword rendering ------------------------------------------

(defun render-type-member (name bits)
  (let ((n (ldb (byte 4 8) bits))
        (background (ldb (byte 8 0) bits)))
    (with-output-to-string (s)
      (format s "~A (" name)
      (let ((first t))
        (dotimes (i 8)
          (when (logbitp i background)
            (unless first (write-char #\Space s))
            (setf first nil)
            (write-string (aref *data-type-names* (mod (+ (* 4 n) i) 64)) s))))
      (write-char #\) s))))

(defun render-block (name bits)
  (let ((operand (insn-operand10 bits)))
    (format nil "~A ~A~:[~; FIXNUM-ONLY~]~:[~; SET-CDR-NEXT~]~
~:[~; INHIBIT-PREFETCH~]~:[~; NO-INCREMENT~]"
            name
            (let ((cycle (ldb (byte 4 6) operand)))
              (if (< cycle (length *memory-cycle-types*))
                  (aref *memory-cycle-types* cycle)
                  (format nil "CYCLE-~D" cycle)))
            (logbitp 5 operand) (logbitp 4 operand)
            (logbitp 3 operand) (logbitp 2 operand))))

(defun render-register (operand)
  (let ((pair (assoc operand *internal-registers*)))
    (if pair
        (format nil "~A (#o~O)" (cdr pair) operand)
        (format nil "#o~O" operand))))

(defparameter *trap-branch-names*
  ;; Branch-true/false group with operand 0 (branch-to-self = trap),
  ;; disassemble.lisp's DBG:OPCODE-SELECT table.  Indexed by opcode - #o60.
  #("TRAP-IF-TRUE" "TRAP-IF-TRUE-EXTRA-POP" "TYPE-TRAP-IF-TRUE"
    "TYPE-TRAP-IF-TRUE-EXTRA-POP" "TRAP-IF-TRUE-NO-POP"
    "UNDEFINED-TRAP-IF-TRUE" "TYPE-TRAP-IF-TRUE-NO-POP"
    "UNDEFINED-TRAP-IF-TRUE"
    "TRAP-IF-FALSE" "TRAP-IF-FALSE-EXTRA-POP" "TYPE-TRAP-IF-FALSE"
    "TYPE-TRAP-IF-FALSE-EXTRA-POP" "TRAP-IF-FALSE-NO-POP"
    "UNDEFINED-TRAP-IF-FALSE" "TYPE-TRAP-IF-FALSE-NO-POP"
    "UNDEFINED-TRAP-IF-FALSE"))

(defun render-packed (bits pc)
  "One packed 18-bit instruction as text (PC = its halfword pc, for
branch targets)."
  (let* ((opcode (insn-opcode bits))
         (spec (aref *packed-ops* opcode)))
    (if (null spec)
        (format nil "UNDEFINED-OP-#o~O operand #o~O"
                opcode (insn-operand10 bits))
        (destructuring-bind (name format &rest flags) spec
          (ecase format
            (:none name)
            (:addr (format nil "~A ~A" name
                           (format-address-operand
                            bits (member :signed flags))))
            (:addr-1 (format nil "~A ~A" name
                             (format-address-operand (1- bits) nil)))
            (:imm10 (format nil "~A ~D" name (insn-operand10 bits)))
            (:branch
             (let ((operand (insn-operand10 bits)))
               (if (and (zerop operand) (<= #o60 opcode #o77))
                   (aref *trap-branch-names* (- opcode #o60))
                   (format nil "~A ~D" name
                           (+ pc (sign-extend operand 10))))))
            (:byte-ldb
             (let ((size (1+ (ldb (byte 5 5) bits)))
                   (rotation (ldb (byte 5 0) bits)))
               (if (and (= opcode #o170) (= size 32) (/= rotation 0))
                   (format nil "ROT ~D~50T;Strange LDB"
                           (sign-extend rotation 5))
                   (format nil "~A (Byte ~D ~D)" name size
                           (ldb (byte 5 0) (- 32 rotation))))))
            (:byte-dpb
             (format nil "~A (Byte ~D ~D)" name
                     (1+ (ldb (byte 5 5) bits)) (ldb (byte 5 0) bits)))
            (:type-member (render-type-member name bits))
            (:block (render-block name bits))
            (:lexical
             (format nil "~A-~D ~A" name (logand opcode #o7)
                     (format-address-operand bits nil)))
            (:finish-call
             (let ((n-ish (member opcode (list #o134 #o135)))
                   (apply-p (member opcode (list #o135 #o137))))
               (format nil "FINISH-CALL~:[~;-APPLY~]-~A-~A"
                       apply-p
                       (if n-ish
                           (format nil "~D" (1- (insn-operand8 bits)))
                           "STACK")
                       (aref *value-dispositions* (ldb (byte 2 8) bits)))))
            (:return-single
             (let ((operand (insn-operand10 bits)))
               (format nil "RETURN-SINGLE-~A"
                       (case operand
                         (#o1000 "STACK")
                         (#o1041 "T")
                         (#o1040 "NIL")
                         (t (render-register operand))))))
            (:unbind
             (let ((operand (insn-operand8 bits))
                   (mode (insn-mode bits)))
               (cond ((and (= mode 3) (= operand 1)) "UNBIND")
                     ((and (= mode 2) (zerop operand)) "UNBIND-N-TOS")
                     (t (format nil "UNBIND-N ~A"
                                (format-address-operand bits nil))))))
            (:catch-open
             (let ((operand (insn-operand10 bits)))
               (if (logbitp 0 operand)
                   "UNWIND-PROTECT-OPEN"
                   (format nil "CATCH-OPEN-~A"
                           (aref *value-dispositions*
                                 (ldb (byte 2 6) operand))))))
            (:register (format nil "~A ~A" name
                               (render-register (insn-operand10 bits))))
            (:entry
             ;; Reached only when an entry opcode appears at an odd pc or
             ;; past word 0; DISASSEMBLE-WORD handles the normal whole-word
             ;; case.
             (format nil "~A ~D" name (insn-operand10 bits))))))))

(defun render-entry-word (tag data)
  "The whole-word entry instruction: even half = ENTRY-REST-[NOT-]ACCEPTED
with min args, odd half low bits = max args (sysdef.lisp
%%ENTRY-INSTRUCTION-MIN/MAX)."
  (declare (ignore tag))
  (let* ((even (q-even-instruction data))
         (min (ldb (byte 8 0) even))
         (max (ldb (byte 8 18) data))
         (rest-accepted (not (logbitp 10 even)))) ; #o176, low opcode bit 0
    (format nil "ENTRY: ~D REQUIRED, ~D OPTIONAL~:[~;, REST ARG~]"
            (- min 2) (- max min) rest-accepted)))

;;; ---- Per-function walker ------------------------------------------------

(defun entry-instruction-p (bits)
  (member (insn-opcode bits) (list #o176 #o177)))

(defun disassemble-cfun (w rec fnmap stream)
  "Print REC's instructions to STREAM, one line per instruction."
  (let* ((cca (cfun-cca rec))
         (code-words (- (cfun-total rec) (cfun-suffix rec) 2)))
    ;; A donor name (--names) leads; the world's own name rendering (for
    ;; Minima worlds, the dangling build-space marker) stays visible after it.
    (format stream "~&#x~8,'0X ~@[~A ~]~A~@[~50T;~D code words, suffix ~D~]~%"
            (cfun-fn rec) (fnmap-donor-name fnmap (cfun-fn rec))
            (render-fn-name rec) code-words (cfun-suffix rec))
    (dotimes (i code-words)
      (let ((vma (+ cca 2 i))
            (pc (* 2 i)))
        (multiple-value-bind (tag data) (world-q w vma)
          (cond
            ((null tag)
             (format stream "~4D      #<UNMAPPED>~%" pc))
            ((>= (tag-type tag) +type-packed-instruction-low+)
             ;; Sequencing markers, as Genera prints them: cdr 3 is the
             ;; full-word-follows glue (even half falls into the next word,
             ;; "++"; the odd half skips over it, "+++"); cdr 2 runs
             ;; backwards ("-"); cdr 1 (fence) inside the code region means
             ;; the header lied.
             (let* ((seq (ash tag -6))
                    (even (q-even-instruction data))
                    (odd (q-odd-instruction tag data))
                    (even-mark (case seq (0 "   ") (1 "###") (2 "-  ") (3 "++ ")))
                    (odd-mark  (case seq (0 "   ") (1 "###") (2 "-  ") (3 "+++"))))
               (cond ((and (zerop i) (entry-instruction-p even))
                      (format stream "~4D      ~A~%"
                              pc (render-entry-word tag data)))
                     (t
                      (format stream "~4D ~A ~A~%"
                              pc even-mark (render-packed even pc))
                      (format stream "~4D ~A ~A~%"
                              (1+ pc) odd-mark (render-packed odd (1+ pc)))))))
            (t
             ;; Full-word instruction (one line, even pc).
             (let ((type (tag-type tag)))
               (format stream "~4D     ~A~%" pc
                       (case type
                         (#.+type-evcp+
                          (format nil "PUSH-INDIRECT ~A"
                                  (render-locative w data fnmap)))
                         ((#.+type-call-compiled-even+
                           #.+type-call-compiled-even-prefetch+
                           #.+type-call-compiled-odd+
                           #.+type-call-compiled-odd-prefetch+)
                          (format nil "START-CALL-DIRECT~:[~;-PREFETCH~] ~A"
                                  (member type
                                          (list +type-call-compiled-even-prefetch+
                                                +type-call-compiled-odd-prefetch+))
                                  (render-call-target
                                   fnmap data
                                   (member type
                                           (list +type-call-compiled-odd+
                                                 +type-call-compiled-odd-prefetch+)))))
                         ((#.+type-call-indirect+
                           #.+type-call-indirect-prefetch+)
                          (format nil "START-CALL-INDIRECT~:[~;-PREFETCH~] ~A"
                                  (= type +type-call-indirect-prefetch+)
                                  (render-locative w data fnmap)))
                         ((#.+type-call-generic+
                           #.+type-call-generic-prefetch+)
                          (format nil "START-CALL-GENERIC~:[~;-PREFETCH~] #x~8,'0X"
                                  (= type +type-call-generic-prefetch+) data))
                         (#.+type-native-instruction+
                          (format nil "NATIVE-INSTRUCTION #x~8,'0X" data))
                         (t (format nil "PUSH-CONSTANT ~A"
                                    (render-q w tag data fnmap)))))))))))))

;;; ---- Driver -------------------------------------------------------------

(defun disasm-world (path &key vma name names (depth 24)
                               (budget *w-decode-limit*)
                               (stream *standard-output*))
  "The `disasm` subcommand.  VMA restricts to the function whose block
contains it; NAME restricts to functions whose rendered name contains the
string (case-insensitive); NAMES is a donor-name file (see
READ-NAMES-FILE) supplying names for worlds whose own name Qs dangle."
  (let* ((model (read-world path))
         (w (windexed model))
         (recs (world-compiled-functions model :depth depth :budget budget))
         (fnmap (make-fnmap recs (and names (read-names-file names))))
         (selected
           (cond (vma (let ((r (fnmap-containing fnmap vma)))
                        (unless r
                          (error "no compiled function contains #x~X" vma))
                        (list r)))
                 (name (remove-if-not
                        (lambda (r)
                          (search name (render-fn-name r fnmap)
                                  :test #'char-equal))
                        recs))
                 (t recs))))
    (format stream "~&;;; ~A: disassembling ~:D of ~:D compiled function~:P~%"
            path (length selected) (length recs))
    (dolist (r selected)
      (terpri stream)
      (disassemble-cfun w r fnmap stream))
    (length selected)))
