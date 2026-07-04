#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""strip-genera-styles.py -- remove Genera character-style "006 escapes" from
Genera Lisp (and other text) source files, preserving the real source text
byte-for-byte.

WHY THIS EXISTS
---------------
When Symbolics Genera saves a :LISP (or other text) buffer that carries
character-style / font / face markup, ZWEI writes that markup into an otherwise
plain-text file using in-band escape sequences introduced by ASCII 0x06 (^F,
the "epsilon" character).  The escape grammar is documented and produced by the
Genera source SYS:IO;STRING-DUMP.LISP (the ":dump-string" escape-coding path);
this script implements the inverse -- it discards the style markup and keeps the
underlying characters.

These escapes make the files annoying to read/diff/grep on a Unix host, and they
carry no semantic meaning for Lisp: they are pure presentation.  Stripping them
yields clean, portable text with identical Lisp semantics.

THE ESCAPE GRAMMAR (reverse-engineered from real data + STRING-DUMP.LISP)
------------------------------------------------------------------------
Every escape is introduced by a single 0x06 byte.  The byte(s) that follow
select the escape variant:

  1. BEGIN MARKER    0x06 0x05 ... '[Begin using 006 escapes]'
     The "start of escape-hood" magic sequence, carrying a format version
     number and a font-attribute blob.  Example from
     SYS:SYS;PKGDCL.LISP line 5351 (also io/string-dump.lisp line 1), shown
     with control bytes escaped:

         \x06\x05D,#TD1Ps\x1eT\x02[Begin using 006 escapes]

     The whole run from 0x06 0x05 through the closing ']' is markup and is
     removed.  (No '[End using 006 escapes]' variant exists anywhere in the
     rel-8-5 tree; escaping simply continues, and style 0 is the plain
     default.)

  2. TYPE DEFINITION   0x06 ( ... )
     A "character type definition" list, e.g.

         \x06(1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI")

     Format: (<n> <bits> (<char-set> <offset>) (<family> <face> <size>)
              <default-font>).  The list is balance-tracked with string
     awareness (it contains "..."-quoted font names which may hold parens).
     0x06 (nn) is also how types 10+ are *selected* -- same rule, same removal.

  3. STYLE SELECT      0x06 <digit 0-9>
     Selects a previously-defined character type 0..9 (type 0 is always the
     plain default).  Exactly two bytes: the 0x06 and the single ASCII digit.
     (Text following the digit is real content, even when it starts with '-';
     the Rel-6.2 "-frobs after #/9" reader tolerance is never emitted here.)

  4. GO BACK           0x06 *
     "(LMITI) *" -- pop back to the previous style level.  Two bytes.

  5. LITERAL EPSILON   0x06 0x06  -->  a single real 0x06 in the source
     This is how an actual epsilon character is encoded when it would
     otherwise be read as an escape introducer.  We collapse the pair to ONE
     0x06 (it is real content, not markup).

WHAT IS *NOT* AN ESCAPE (kept byte-for-byte)
--------------------------------------------
A bare 0x06 that is NOT followed by one of the escape operators above
(0x05, 0x06, '(', a digit, or '*') is a *literal epsilon character* that the
dumper left un-doubled because its context makes it unambiguous to Genera's
reader.  Two real patterns occur in the rel-8-5 tree, and BOTH must be
preserved:

  * Lisp character literals: `#/\x06)` reads as `#\<epsilon>` followed by the
    genuine closing paren.  Removing 0x06)  would delete a real ')' and break
    paren balance.
  * Epsilon glyph in comments/docstrings: e.g.  `"sign \x06 {0,-1}"` (epsilon
    as set-membership) or `;\x06{:NORMAL, ...}`.  These are ordinary text.

Likewise, legitimate high-bit Symbolics glyph bytes (e.g. a not-equals sign in
a comment) are never introduced by 0x06 and are therefore never touched.

If this script ever meets a 0x06 whose following byte cannot be classified
under the grammar above -- an unterminated begin-marker or an unbalanced type
definition -- it raises UnparsableEscape and refuses to write, rather than
guessing.

MODES
-----
  (default)              dry-run: report files + per-variant escape counts,
                         write nothing.
  --stdout FILE          write the stripped bytes of one FILE to stdout.
  --in-place FILE...     strip the named files in place.  Before modifying a
                         file, copy it to FILE.style-orig (never overwriting an
                         existing backup).  Only files that actually contain a
                         0x06 are touched.
  --tree DIR             like --in-place but over every text file under DIR
                         (binary files -- e.g. *.vbin -- are skipped).

All file I/O is BINARY: no encoding, newline, or whitespace munging.  The only
change is removal/collapse of 0x06 escape sequences per the grammar above.
"""

import os
import sys
import argparse

EPS = 0x06          # ^F, the escape introducer / epsilon character
BEGIN = 0x05        # follows 0x06 to open the '[Begin using 006 escapes]' run
LPAREN = ord('(')
RPAREN = ord(')')
QUOTE = ord('"')
STAR = ord('*')
RBRACK = ord(']')


class UnparsableEscape(Exception):
    """Raised when a 0x06 sequence does not fit the known grammar."""


def _scan_type_def(data, i):
    """i points at the '(' after 0x06.  Return index just past the matching
    ')', balance-tracking with string awareness.  Raise if unbalanced."""
    depth = 0
    j = i
    n = len(data)
    in_str = False
    while j < n:
        ch = data[j]
        if in_str:
            if ch == QUOTE:
                in_str = False
        elif ch == QUOTE:
            in_str = True
        elif ch == LPAREN:
            depth += 1
        elif ch == RPAREN:
            depth -= 1
            if depth == 0:
                return j + 1
        j += 1
    raise UnparsableEscape("unbalanced 0x06 type-definition list")


def strip_bytes(data, counts=None):
    """Return the stripped bytes of `data`.  If `counts` (a dict) is given,
    accumulate per-variant counts into it.  Raise UnparsableEscape on any
    0x06 that does not match the grammar."""
    out = bytearray()
    i = 0
    n = len(data)

    def bump(key):
        if counts is not None:
            counts[key] = counts.get(key, 0) + 1

    while i < n:
        c = data[i]
        if c != EPS:
            out.append(c)
            i += 1
            continue

        nb = data[i + 1] if i + 1 < n else None

        if nb == BEGIN:
            # 0x06 0x05 ... ']'  -- the begin marker run
            j = data.find(RBRACK, i)
            if j < 0:
                raise UnparsableEscape(
                    "unterminated '[Begin using 006 escapes]' marker at "
                    "offset %d" % i)
            bump('begin_marker')
            i = j + 1
            continue

        if nb == EPS:
            # 0x06 0x06 -> one real epsilon
            bump('literal_epsilon')
            out.append(EPS)
            i += 2
            continue

        if nb == LPAREN:
            j = _scan_type_def(data, i + 1)
            bump('type_def')
            i = j
            continue

        if nb is not None and 0x30 <= nb <= 0x39:
            # 0x06 <digit> -> style select, exactly two bytes
            bump('style_select')
            i += 2
            continue

        if nb == STAR:
            bump('go_back')
            i += 2
            continue

        # Bare 0x06 not followed by an escape operator: a literal epsilon
        # character (char literal `#/^F`, or an epsilon glyph in text).  Keep
        # the 0x06; the following byte is ordinary content and is handled on
        # the next loop iteration.
        bump('literal_epsilon_bare')
        out.append(EPS)
        i += 1

    return bytes(out)


def _read(path):
    with open(path, 'rb') as f:
        return f.read()


def _looks_binary(data):
    """Heuristic: treat as binary if it contains a NUL byte.  Genera .vbin and
    other compiled artifacts contain NULs; text sources (even with 0x06
    markup) do not."""
    return b'\x00' in data


def do_report(paths):
    grand = {}
    any_hit = False
    for p in paths:
        try:
            data = _read(p)
        except (IOError, OSError) as e:
            sys.stderr.write("skip (unreadable): %s (%s)\n" % (p, e))
            continue
        if EPS not in data:
            continue
        if _looks_binary(data):
            sys.stderr.write("skip (binary): %s\n" % p)
            continue
        counts = {}
        try:
            stripped = strip_bytes(data, counts)
        except UnparsableEscape as e:
            sys.stderr.write("UNPARSABLE: %s: %s\n" % (p, e))
            continue
        any_hit = True
        removed = len(data) - len(stripped)
        detail = ", ".join("%s=%d" % (k, counts[k]) for k in sorted(counts))
        print("%s  (-%d bytes)  [%s]" % (p, removed, detail))
        for k, v in counts.items():
            grand[k] = grand.get(k, 0) + v
    if any_hit:
        print("--- totals: " +
              ", ".join("%s=%d" % (k, grand[k]) for k in sorted(grand)))
    else:
        print("(no files with 006 escapes)")


def do_stdout(path):
    data = _read(path)
    if _looks_binary(data):
        sys.stderr.write("refusing: %s looks binary\n" % path)
        return 2
    stripped = strip_bytes(data)
    sys.stdout.buffer.write(stripped)
    return 0


def _backup_path(path):
    return path + ".style-orig"


def strip_in_place(path):
    """Strip `path` in place if it contains 0x06.  Returns (changed, removed).
    Makes a .style-orig backup first; never overwrites an existing backup."""
    data = _read(path)
    if EPS not in data:
        return (False, 0)
    if _looks_binary(data):
        sys.stderr.write("skip (binary): %s\n" % path)
        return (False, 0)
    stripped = strip_bytes(data)  # may raise UnparsableEscape -> abort file
    if stripped == data:
        return (False, 0)
    bak = _backup_path(path)
    if os.path.exists(bak):
        raise RuntimeError(
            "backup already exists, refusing to overwrite: %s" % bak)
    # Write backup first (copy raw bytes), then overwrite original.
    with open(bak, 'wb') as f:
        f.write(data)
    with open(path, 'wb') as f:
        f.write(stripped)
    return (True, len(data) - len(stripped))


def do_in_place(paths):
    changed = 0
    total_removed = 0
    for p in paths:
        try:
            was_changed, removed = strip_in_place(p)
        except UnparsableEscape as e:
            sys.stderr.write("UNPARSABLE (left untouched): %s: %s\n" % (p, e))
            continue
        except (IOError, OSError, RuntimeError) as e:
            sys.stderr.write("error: %s: %s\n" % (p, e))
            continue
        if was_changed:
            changed += 1
            total_removed += removed
            print("stripped %s  (-%d bytes, backup %s)" %
                  (p, removed, _backup_path(p)))
    print("--- %d file(s) changed, %d escape byte(s) removed total" %
          (changed, total_removed))


def walk_tree(root):
    for dirpath, dirnames, filenames in os.walk(root):
        for fn in sorted(filenames):
            if fn.endswith(".style-orig"):
                continue
            yield os.path.join(dirpath, fn)


def files_with_eps(paths):
    """Filter `paths` to those that are readable text files containing 0x06."""
    out = []
    for p in paths:
        if not os.path.isfile(p):
            continue
        try:
            with open(p, 'rb') as f:
                head = f.read()
        except (IOError, OSError):
            continue
        if EPS in head and not _looks_binary(head):
            out.append(p)
    return out


def main(argv):
    ap = argparse.ArgumentParser(
        description="Strip Genera character-style 006 (^F) escapes from text "
                    "source files, preserving real source bytes.")
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--stdout", metavar="FILE",
                   help="write the stripped copy of FILE to stdout")
    g.add_argument("--in-place", nargs="+", metavar="FILE",
                   help="strip the named FILEs in place (backup to "
                        ".style-orig first)")
    g.add_argument("--tree", metavar="DIR",
                   help="strip every text file under DIR in place (backups "
                        "as with --in-place)")
    ap.add_argument("files", nargs="*",
                    help="files to report on (dry-run, default mode)")
    args = ap.parse_args(argv)

    if args.stdout:
        return do_stdout(args.stdout)

    if args.in_place:
        do_in_place(args.in_place)
        return 0

    if args.tree:
        targets = files_with_eps(walk_tree(args.tree))
        do_in_place(targets)
        return 0

    # default: dry-run report
    if not args.files:
        ap.error("no files given (use --stdout / --in-place / --tree, or pass "
                 "files for a dry-run report)")
    do_report(args.files)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
