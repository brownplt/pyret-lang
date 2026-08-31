#!/usr/bin/env bash
# Well-formedness / scope-error parity: compile every inline program from the
# in-suite wf and compile-error tests (extracted at runtime by
# extract-suite-programs.js -- see its header) under BOTH the Pyret-hosted
# compiler (build/phaseA/pyret.jarr) and the TypeScript compiler
# (build/ts-compiler/pyret.js), with default options, and require identical
# exit statuses and diagnostics.
#
# WHY THIS EXISTS: like the type-check corpus (see type-check-parity.sh), the
# in-suite tests import src/arr/compiler/*, so building them with the TS
# compiler only proves TS codegen of the .arr well-formedness checker. This
# harness runs the same accumulated corpus through well-formed.ts /
# resolve-scope.ts. Compile-only: parity is on the compiler's verdict and
# message bytes, not on runtime behavior (the corpus's `cok` success
# programs are asserted to compile cleanly in both).
#
# ENGINE: compile servers driven over the npm client's -serve protocol --
# one boot per compiler instead of one per program (see type-check-parity.sh
# for the rationale and what this means for the compared surface).
#
# Run from the pyret-lang root (lang/):
#   bash src/ts-compiler/tests/wf-parity.sh          (or `make ts-wf-parity`)

set -u
cd "$(dirname "$0")/../../.."   # lang/

SOURCES=(tests/pyret/tests/test-well-formed.arr
         tests/pyret/tests/test-compile-errors.arr)
WORK=build/ts-compiler/wf-parity
DRIVER=src/ts-compiler/tests/parity-serve-driver.js

rm -rf "$WORK/programs" "$WORK/out"
mkdir -p "$WORK/programs" "$WORK/out"
. src/ts-compiler/tests/parity-serve-lib.sh

# The generated .arr programs must not outlive the run -- even on failure:
# tests/pyret/tests/test-pprint.arr walks the whole lang/ tree at suite
# runtime and adds 3 tests per .arr file it finds, so leftover generated
# programs silently inflate the main suite's test count (build artifacts
# included; see the cache-warm comment in the Makefile for the same issue).
# The manifest and per-program .out/.diff files survive for debugging; to
# reproduce a failing program, re-run this script (extraction is
# deterministic) or find its source at the manifest's file:line.
# stop_servers is chained in because sourcing the lib set a trap above.
trap 'rm -f "$WORK/programs"/p*.arr; stop_servers' EXIT

count=$(node src/ts-compiler/tests/extract-suite-programs.js "$WORK/programs" "${SOURCES[@]}")
echo "extracted $count unique programs from ${SOURCES[*]}"

shopt -s nullglob
programs=("$WORK/programs"/p*.arr)
printf '%s\n' "${programs[@]}" > "$WORK/programs.txt"

start_servers
trap 'rm -f "$WORK/programs"/p*.arr; stop_servers' EXIT

node "$DRIVER" "$ARR_SOCK" "$WORK/programs.txt" "$WORK/out" -arr &
DRIVE_ARR=$!
node "$DRIVER" "$TS_SOCK" "$WORK/programs.txt" "$WORK/out" -ts &
DRIVE_TS=$!
wait "$DRIVE_ARR" || { echo "FAIL: arr driver crashed"; exit 1; }
wait "$DRIVE_TS" || { echo "FAIL: ts driver crashed"; exit 1; }

stop_servers
trap 'rm -f "$WORK/programs"/p*.arr' EXIT

pass=0
fail=0
failed_programs=()

for prog in "${programs[@]}"; do
  base=$(basename "$prog" .arr)
  where=$(grep "^$base " "$WORK/programs/manifest.txt" | head -1)
  stat_a=$(grep "^$base " "$WORK/out/results-arr.txt" | head -1 | awk '{print $2}')
  stat_t=$(grep "^$base " "$WORK/out/results-ts.txt" | head -1 | awk '{print $2}')

  if [ "$stat_a" != "$stat_t" ] || [ "$stat_a" = "ERR" ] || [ -z "$stat_a" ]; then
    echo "FAIL $base ($where): statuses differ or errored (arr=${stat_a:-missing} ts=${stat_t:-missing})"
    echo "  arr tail: $(tail -2 "$WORK/out/$base-arr.out" 2>/dev/null | head -c 300)"
    echo "  ts  tail: $(tail -2 "$WORK/out/$base-ts.out" 2>/dev/null | head -c 300)"
    fail=$((fail+1)); failed_programs+=("$base"); continue
  fi

  if diff -u "$WORK/out/$base-arr.out" "$WORK/out/$base-ts.out" > "$WORK/out/$base.diff"; then
    rm -f "$WORK/out/$base.diff"
    pass=$((pass+1))
  else
    echo "FAIL $base ($where): diagnostics differ (see $WORK/out/$base.diff)"
    fail=$((fail+1)); failed_programs+=("$base")
  fi
done

echo
if [ "$fail" -eq 0 ]; then
  echo "wf parity: $pass passed, $fail failed (of $count)"
else
  echo "wf parity: $pass passed, $fail failed (of $count)"
  echo "failed: ${failed_programs[*]}"
  echo "(map names to sources via $WORK/programs/manifest.txt)"
  exit 1
fi
