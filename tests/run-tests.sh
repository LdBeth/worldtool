#!/bin/sh
# worldtool regression tests: roundtrip every specimen world byte-identically
# and spot-check the VLM_debugger dump against its known decode.
# Run from the linux-vlm repo root or set VLMDIR.

set -e
here="$(dirname "$0")/.."
VLMDIR="${VLMDIR:-$here/../og2vlm}"
WT="$here/worldtool"

fail=0

for f in VLM_debugger Genera-8-5e.vlod Concordia.ilod Initial.vlod pascal.ilod; do
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

# Export -> emit -> compare must also be lossless (exercises the sexp path)
tmp="${TMPDIR:-/tmp}/worldtool-test.$$"
mkdir -p "$tmp"
"$WT" export "$VLMDIR/VLM_debugger" "$tmp/dbg.sexp" "$tmp/dbg.qs" > /dev/null
"$WT" emit "$tmp/dbg.sexp" "$tmp/dbg.out" > /dev/null
cmp "$VLMDIR/VLM_debugger" "$tmp/dbg.out" || { echo "FAIL: export/emit not lossless"; fail=1; }
rm -rf "$tmp"

[ $fail -eq 0 ] && echo "all tests passed" || echo "TESTS FAILED"
exit $fail
