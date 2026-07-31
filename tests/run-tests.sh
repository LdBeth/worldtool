#!/bin/sh
# worldtool regression tests: roundtrip every specimen world byte-identically
# and spot-check the VLM_debugger dump against its known decode.
# Run from the linux-vlm repo root or set VLMDIR.

set -e
here="$(dirname "$0")/.."
VLMDIR="${VLMDIR:-$here/../og2vlm}"
WT="$here/worldtool"

fail=0

for f in VLM_debugger Genera-8-5e.vlod Concordia.vlod Initial.vlod Pascal.vlod; do
    if [ -f "$VLMDIR/$f" ]; then
        "$WT" roundtrip "$VLMDIR/$f" || fail=1
    else
        echo "skip: $VLMDIR/$f not found"
    fi
done

dump=$("$WT" dump "$VLMDIR/VLM_debugger")
echo "$dump" | grep -q "ilod world, 346,880 bytes" || { echo "FAIL: debugger size"; fail=1; }
echo "$dump" | grep -q "wired map (5 entries)" || { echo "FAIL: debugger map count"; fail=1; }
echo "$dump" | grep -q "constant vma #xF8041002 count 1 (#x1) <- q 1C:F8017033" \
    || { echo "FAIL: fepStartup entry"; fail=1; }

# symbols: the debugger's 4 Character arrays decode exactly, and the packed
# fixnum print-names surface the debug-info vocabulary (gc/debug-info.lisp).
syms=$("$WT" symbols "$VLMDIR/VLM_debugger")
echo "$syms" | grep -q "4 character-array strings" || { echo "FAIL: symbols char-array count"; fail=1; }
for s in SCSI-CDROM BASIC-TAPE ACCEPT-TYPE NULL-ACCEPT-TYPE; do
    echo "$syms" | grep -q "^  $s$" || { echo "FAIL: symbols missing char array $s"; fail=1; }
done
echo "$syms" | grep -q "COMPRESSED-PNAME-ARRAY" \
    || { echo "FAIL: symbols missing packed debug-info pname"; fail=1; }

# functions: compiled-function census.  The debugger world's symbols keep
# compressed pnames, so every name decodes opaque: 677 compound specs are
# lists of opaque symbol markers, the rest fail -- the census still proves
# the header scan, suffix bounds, and cut/opaque accounting.
funs=$("$WT" functions "$VLMDIR/VLM_debugger")
echo "$funs" | grep -q "1,729 compiled-function candidates" \
    || { echo "FAIL: functions candidate count"; fail=1; }
echo "$funs" | grep -q "names: 0 simple symbols, 677 compound function specs, 0 instance-named (method objects), 1,052 nil/failed" \
    || { echo "FAIL: functions name classes"; fail=1; }
echo "$funs" | grep -q "suffix decodes: 0 clean, 0 depth-cut, 0 budget-cut, 1,729 with opaque objects, 0 with unmapped Qs" \
    || { echo "FAIL: functions cut accounting"; fail=1; }

# disasm: donor names (vlm-debugger.names, recovered from the distribution
# world's FEPComm slots and trap-vector grafts) label the kernel entry points
# whose own name Qs dangle in Minima build space.
dis=$("$WT" disasm "$VLMDIR/VLM_debugger" --names "$here/vlm-debugger.names" --vma F8010F3C)
echo "$dis" | grep -q "#xF8010F3C WIRED-FORMAT #<Q 58:FA060815>" \
    || { echo "FAIL: disasm donor name"; fail=1; }

# vbin: decode Genera compiler output (skipped when the source tree is absent)
SYSDIR="${SYSDIR:-/Users/ldbeth/Public/symbolics/rel-8-5/sys}"
if [ -f "$SYSDIR/io/lmini.vbin" ]; then
    vb=$("$WT" vbin "$SYSDIR/io/lmini.vbin" "$SYSDIR/sys/ltop.vbin" --trace)
    echo "$vb" | grep -q "lmini.vbin: BIN version 5, 3989 words (0 padding), 913 table slots" \
        || { echo "FAIL: lmini vbin decode"; fail=1; }
    echo "$vb" | grep -q "SETQ MINI-DESTINATION-ADDRESS 257" \
        || { echo "FAIL: lmini patched setq missing"; fail=1; }
    echo "$vb" | grep -q "2 files, 0 failures" || { echo "FAIL: vbin decode failures"; fail=1; }

    # disasm on a .vbin: the wire-format path.  INITIALIZATION-LIST-SYMBOL-P
    # must render the same instruction stream the world disassembler prints
    # for it out of Genera-8-5.vlod (verified A/B 2026-07-27).
    vdis=$("$WT" disasm "$SYSDIR/sys/ltop.vbin" --name initialization-list-symbol-p)
    echo "$vdis" | grep -q "disassembling 1 of 27 compiled functions" \
        || { echo "FAIL: vbin disasm selection"; fail=1; }
    echo "$vdis" | grep -q "START-CALL-INDIRECT-PREFETCH #'GET" \
        || { echo "FAIL: vbin disasm indirect call"; fail=1; }
    echo "$vdis" | grep -q "PUSH-CONSTANT INITIALIZATION-LIST" \
        || { echo "FAIL: vbin disasm constant"; fail=1; }
else
    echo "skip: $SYSDIR .vbins not found"
fi

# Export -> emit -> compare must also be lossless (exercises the sexp path)
tmp="${TMPDIR:-/tmp}/worldtool-test.$$"
mkdir -p "$tmp"
"$WT" export "$VLMDIR/VLM_debugger" "$tmp/dbg.sexp" "$tmp/dbg.qs" > /dev/null
"$WT" emit "$tmp/dbg.sexp" "$tmp/dbg.out" > /dev/null
cmp "$VLMDIR/VLM_debugger" "$tmp/dbg.out" || { echo "FAIL: export/emit not lossless"; fail=1; }

# Cold-load generator stage checks (structural diff against the unpatched
# ground-truth world when present)
coldref=""
[ -f "$VLMDIR/Genera-8-5.vlod" ] && coldref="--reference $VLMDIR/Genera-8-5.vlod"
coldsys=""
[ -d "$SYSDIR" ] && coldsys="--sys $SYSDIR"
"$WT" coldtest "$here/cold-layout.sexp" "$tmp" $coldref $coldsys || { echo "FAIL: coldtest"; fail=1; }
"$WT" roundtrip "$tmp/cold-skeleton.ilod" || { echo "FAIL: cold skeleton roundtrip"; fail=1; }

# Replay: the frozen reference data must carry the full gate suite without
# the distribution world present.  A replay MISS (unrecorded datum) means a
# gate or the cold set changed: re-run `worldtool extract-reference`.
if [ -f "$here/reference-data.lisp" ]; then
    "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
        --reference-data "$here/reference-data.lisp" $coldsys \
        || { echo "FAIL: coldtest replay (reference-data)"; fail=1; }
else
    echo "skip: reference-data.lisp not found"
fi

# Negative test: a gate that has never been seen to fail is a comment.
# Reintroduce the defect with `--defeat NAME' and require a RED build that
# NAMES the defect.  One entry per *COLD-DEFEATABLE-FIXES* name.
#
# snap-cell-refs: stock Genera's loader snaps invisible pointers when it
# builds compiled-code constants.  Unsnapped, a DTP-LOCATIVE constant names
# the heap value cell holding the DEFWIREDVAR one-q-forward, and raw
# %MEMORY-WRITE clobbers the forward itself -- SI:*INTERRUPT-TASK-FREE-LIST*
# splits in two and QLD dies with "Interrupt task queue is full".
if [ -n "$coldsys" ] && [ -f "$here/reference-data.lisp" ]; then
    neg="${TMPDIR:-/tmp}/worldtool-negtest.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat snap-cell-refs > "$neg" 2>&1; then
        echo "FAIL: negative test (snap-cell-refs) built GREEN -- \
check-cell-ref-snapping does not fire"; fail=1
    fi
    grep -q "FAIL cell-ref snapping" "$neg" \
        || { echo "FAIL: negative test (snap-cell-refs): the failure was \
not check-cell-ref-snapping"; fail=1; }
    grep -q "ENQUEUE-INTERRUPT-TASK.*unsnapped cell reference" "$neg" \
        || { echo "FAIL: negative test (snap-cell-refs): gate did not name \
ENQUEUE-INTERRUPT-TASK"; fail=1; }
    rm -f "$neg"

    # lambda-macro-cells: DEFLAMBDA-MACRO fdefines the keyword-headed fspec
    # (:LAMBDA-MACRO name); its handler stores the definition as a PROPERTY
    # on NAME's plist.  Routed to the generic detached-cell branch instead,
    # NAMED-LAMBDA's plist stays NIL, LAMBDA-MACRO-CALL-P returns NIL, and
    # the first interpreted (NAMED-LAMBDA ...) expander FERRORs -- QLD
    # attempt 7, loading SYS:SYS2;TABLES.VBIN.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-lm.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat lambda-macro-cells > "$neg" 2>&1; then
        echo "FAIL: negative test (lambda-macro-cells) built GREEN -- \
check-lambda-macro-cells does not fire"; fail=1
    fi
    grep -q "FAIL lambda-macro cells" "$neg" \
        || { echo "FAIL: negative test (lambda-macro-cells): the failure \
was not check-lambda-macro-cells"; fail=1; }
    grep -q "FAIL lambda-macro cells.*NAMED-LAMBDA" "$neg" \
        || { echo "FAIL: negative test (lambda-macro-cells): gate did not \
name NAMED-LAMBDA"; fail=1; }
    rm -f "$neg"

    # bignum-encoding: an Ivory bignum is two's complement with an implied
    # SIGN WORD (header bit 27 = a leading -2^(32*len) term), not
    # sign-magnitude.  Encoded sign-magnitude, SI:*BIGNUM-2SETZ* comes out
    # -(2^64 - 2^32) instead of -2^32 and VERIFY-OPEN-CODED-CONSTANTS
    # signals INLINE-CONSTANT-VALUE-CHANGED -- QLD attempt 8, loading
    # SYS:SYS2;BIGNUM.VBIN.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-bn.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat bignum-encoding > "$neg" 2>&1; then
        echo "FAIL: negative test (bignum-encoding) built GREEN -- \
check-bignum-encoding does not fire"; fail=1
    fi
    grep -q "FAIL bignum @" "$neg" \
        || { echo "FAIL: negative test (bignum-encoding): the failure was \
not check-bignum-encoding"; fail=1; }
    grep -q "4294967296" "$neg" \
        || { echo "FAIL: negative test (bignum-encoding): gate did not name \
the intended value -4294967296 (*BIGNUM-2SETZ*)"; fail=1; }
    rm -f "$neg"

    # zl-slash-escape: PKGDCL is Zetalisp syntax, where / is the
    # single-character escape and \ an ordinary constituent.  Read with the
    # Common Lisp model, LISP exports "//" instead of "/", so the QLD-time
    # (INTERN "/" PROCESS) loading SYS:SCHEDULER;WAIT-FUNCTIONS.VBIN interns
    # a fresh symbol with an unbound function cell -- Error trap 71 at PC 6
    # in ZL:FSYMEVAL, QLD attempt 9.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-slash.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat zl-slash-escape > "$neg" 2>&1; then
        echo "FAIL: negative test (zl-slash-escape) built GREEN -- \
check-pkgdcl-escapes does not fire"; fail=1
    fi
    grep -q "FAIL pkgdcl escape" "$neg" \
        || { echo "FAIL: negative test (zl-slash-escape): the failure was \
not check-pkgdcl-escapes"; fail=1; }
    grep -q "WAIT-FUNCTIONS" "$neg" \
        || { echo "FAIL: negative test (zl-slash-escape): gate did not name \
the QLD victim SYS:SCHEDULER;WAIT-FUNCTIONS.VBIN"; fail=1; }
    rm -f "$neg"

    # uncomposable-cfms: the LANGUAGE-TOOLS files queue COMPILE-FLAVOR-
    # METHODS-LOAD-TIME forms on CONDITION flavors whose component closure
    # reaches the QLD-warm ERROR (lambda-list.lisp's six LAMBDA-LIST-*
    # errors) and NO-ACTION-MIXIN (mapforms.lisp's FORM-NOT-UNDERSTOOD).
    # Left on the deferred list, COMPOSE-FLAVOR-COMBINATION WARNs "the
    # components could not be fully determined" pre-banner, and any WARN
    # before the banner is fatal by design (streams unbound) -- QLD
    # attempt 10.  A CFM is a pure composition optimization, so the
    # finalize pass withholds them and the flavors compose warm.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-cfm.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat uncomposable-cfms > "$neg" 2>&1; then
        echo "FAIL: negative test (uncomposable-cfms) built GREEN -- \
check-deferred-flavor-composition does not fire"; fail=1
    fi
    grep -q "composes undefined component" "$neg" \
        || { echo "FAIL: negative test (uncomposable-cfms): the failure was \
not check-deferred-flavor-composition"; fail=1; }
    grep -q "LAMBDA-LIST-SYNTAX-ERROR" "$neg" \
        || { echo "FAIL: negative test (uncomposable-cfms): gate did not \
name LAMBDA-LIST-SYNTAX-ERROR"; fail=1; }
    rm -f "$neg"

    # static-exports: the Genera compiler rewrites every top-level
    # (EXPORT '(...)) at dump time into (EXPORT (SI:%LIST-N n (INTERN
    # "NAME" (FIND-PACKAGE-FOR-SYNTAX "PKG" :COMMON-LISP)) ...)), and
    # SI:FIND-PACKAGE-FOR-SYNTAX lives in the WARM SYS:SYS;LISP-SYNTAX.
    # Deferred rather than discharged against PKGDCL, the five
    # LANGUAGE-TOOLS forms (clcp/mapforms, clcp/annotate, clcp/setf)
    # replay pre-banner and FSYMEVAL an unbound function cell -- Error
    # trap 71, QLD attempt 11.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-export.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat static-exports > "$neg" 2>&1; then
        echo "FAIL: negative test (static-exports) built GREEN -- \
check-deferred-callee-boundness does not fire"; fail=1
    fi
    grep -q "deferred form calls unbound" "$neg" \
        || { echo "FAIL: negative test (static-exports): the failure was \
not check-deferred-callee-boundness"; fail=1; }
    grep -q "deferred form calls unbound.*FIND-PACKAGE-FOR-SYNTAX" "$neg" \
        || { echo "FAIL: negative test (static-exports): gate did not name \
FIND-PACKAGE-FOR-SYNTAX"; fail=1; }
    rm -f "$neg"

    # macroexpand-compiler-hooks: SI:MACROEXPAND-1-INTERNAL consults the
    # WARM compiler protocol on every interpreted macro expansion --
    # (NULL (COMPILER:GET-PHASE-1-HANDLER COMPILER:*COMPILER* FN)) and the
    # matching GET-TRANSFORMERS (macroexpand.lisp:130-137), reached because
    # the interpreter always passes DONT-EXPAND-SPECIAL-FORMS true.  Left
    # unbound, the first interpreted macro call reads a DTP-NULL cell:
    # Error trap 57 (nullfw) at PC 224 in MACROEXPAND-1-INTERNAL, QLD
    # attempt 12 loading SYS:SCHEDULER;LOCKS.VBIN.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-mxhooks.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat macroexpand-compiler-hooks > "$neg" 2>&1; then
        echo "FAIL: negative test (macroexpand-compiler-hooks) built GREEN \
-- check-macroexpand-compiler-hooks does not fire"; fail=1
    fi
    grep -q "FAIL macroexpand compiler hooks" "$neg" \
        || { echo "FAIL: negative test (macroexpand-compiler-hooks): the \
failure was not check-macroexpand-compiler-hooks"; fail=1; }
    grep -q "GET-PHASE-1-HANDLER" "$neg" \
        || { echo "FAIL: negative test (macroexpand-compiler-hooks): gate \
did not name COMPILER:GET-PHASE-1-HANDLER"; fail=1; }
    grep -q "MACROEXPAND-1-INTERNAL" "$neg" \
        || { echo "FAIL: negative test (macroexpand-compiler-hooks): gate \
did not name SI:MACROEXPAND-1-INTERNAL"; fail=1; }
    rm -f "$neg"

    # block-write-functions: a from-scratch world's flavors get a
    # different (legitimate) instance-variable storage order than the
    # Symbolics build world's, so VALIDATE-CONSTRUCTOR-FUNCTIONS
    # (flavor/make.lisp:953) rejects the vbin's dumped
    # CONSTRUCTOR-DERIVATION and Genera correctly regenerates the
    # constructors -- INTERPRETED, since COMPILE-FUNCTION-LIST with no
    # compiler is (MAPC #'EVAL forms).  The digested body calls
    # SI:%BLOCK-n-WRITE, an Ivory instruction (DEFOPCODE,
    # i-sys/opdef.lisp:287) no Genera world defines as a function: Error
    # trap 71, FSYMEVAL of #'SI:%BLOCK-1-WRITE, QLD attempt 13 loading
    # SYS:SCHEDULER;COMETH.VBIN (PROCESS-INITIALIZE -> MAKE-PROCESS).
    # The graft has to be COMPILED: an unbound slot is written as
    # (SI:%BLOCK-WRITE 1 (SI:%SET-TAG 'VAR DTP-NULL)), and an interpreted
    # callee's argument-taking reads that DTP-NULL Q through the data-read
    # barrier -- trap 71 again, in SI:APPLY-LAMBDA (QLD attempt 14).
    # And it cannot write through the REAL BAR-1 either: every Ivory
    # allocation instruction re-primes BAR-1 and the interpreter conses
    # between the constructor's write calls, so the slots scattered into
    # fresh conses and the first instance-variable read trapped 71 (QLD
    # attempt 15).  The fix is the shadow-cursor protocol -- six compiled
    # wrappers keeping the cursor in SI:*INTERPRETED-BAR-n*.  Defeated,
    # nothing points at the wrappers: the three redirected names still
    # hold their originals, the -PRIMITIVE aliases stay DTP-NULL, and
    # SI:%BLOCK-1-WRITE is not even interned (the graft is what interns
    # it) -- 10 gate failures, the first naming %BLOCK-1-WRITE.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-blockwrite.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat block-write-functions > "$neg" 2>&1; then
        echo "FAIL: negative test (block-write-functions) built GREEN -- \
check-block-write-functions does not fire"; fail=1
    fi
    grep -q "FAIL block write functions" "$neg" \
        || { echo "FAIL: negative test (block-write-functions): the \
failure was not check-block-write-functions"; fail=1; }
    grep -q "FAIL block write functions.*%BLOCK-1-WRITE" "$neg" \
        || { echo "FAIL: negative test (block-write-functions): gate did \
not name SI:%BLOCK-1-WRITE"; fail=1; }
    rm -f "$neg"

    # opcode-symbols: SYS:I-SYS;OPDEF.VBIN names every Ivory instruction
    # and built-in PNAME-ONLY (BIN-OP-SYMBOL, "intern in the file
    # package"), and in stock Genera every one of them is already a
    # baked cold symbol -- CAR-LOCAL at 800C2264, home SYSTEM -- that
    # ILC (:USE COMPILER SYSTEM GLOBAL) inherits.  In a world that lacks
    # the name the vbin read mints an ILC twin and ADD-OPCODE's fallback
    # (i-compiler/i-instruction-set.lisp:108-150) INTERNs another into
    # SYSTEM, which is :EXTERNAL-ONLY (pkgdcl.lisp:4114): the export
    # fires on the spot and QLD attempt 16 died with
    # #<NAME-CONFLICT-IN-EXPORT> Exporting #:CAR-LOCAL from package
    # SYSTEM would cause name conflict in ILC.  CAR-LOCAL is only the
    # first :FUNCTION name in the file; defeated, 110 of the 239
    # instruction/built-in names are missing and the gate names
    # CAR-LOCAL first.
    neg="${TMPDIR:-/tmp}/worldtool-negtest-opcodesyms.$$"
    if "$WT" coldtest "$here/cold-layout.sexp" "$tmp" \
            --reference-data "$here/reference-data.lisp" $coldsys \
            --defeat opcode-symbols > "$neg" 2>&1; then
        echo "FAIL: negative test (opcode-symbols) built GREEN -- \
check-opcode-symbols does not fire"; fail=1
    fi
    grep -q "FAIL opcode symbols" "$neg" \
        || { echo "FAIL: negative test (opcode-symbols): the failure was \
not check-opcode-symbols"; fail=1; }
    grep -q "FAIL opcode symbols.*CAR-LOCAL" "$neg" \
        || { echo "FAIL: negative test (opcode-symbols): gate did not \
name CAR-LOCAL"; fail=1; }
    rm -f "$neg"
else
    echo "skip: negative tests need --sys and reference-data.lisp"
fi
rm -rf "$tmp"

[ $fail -eq 0 ] && echo "all tests passed" || echo "TESTS FAILED"
exit $fail
