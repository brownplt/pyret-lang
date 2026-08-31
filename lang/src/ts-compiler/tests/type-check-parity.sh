#!/usr/bin/env bash
# Type-checker parity: compile every program in tests/type-check/
# {good,bad,should,should-not}/ with -type-check under BOTH the Pyret-hosted
# compiler (build/phaseA/pyret.jarr) and the TypeScript compiler
# (build/ts-compiler/pyret.js), and require identical exit statuses and
# diagnostics.
#
# WHY THIS EXISTS: the in-suite type-check tests (tests/type-check/main.arr)
# import src/arr/compiler/* directly, so building THAT suite with the TS
# compiler only proves the TS compiler can compile the .arr type checker --
# it never executes type-check.ts. This harness runs the same corpus through
# type-check.ts itself, transferring the corpus's established expectations
# to the port. Compile-only: runtime behavior of the good programs is the
# main suite's job.
#
# ENGINE: both compilers run as compile SERVERS (the npm client's -serve
# protocol) and the corpus is driven over the socket -- one boot per
# compiler instead of one per program, which cut this harness from ~10min
# to ~2min. Diagnostics are the echo-log/echo-err frames the servers send
# (rendered by the same render-reason code the CLI uses), so this also pins
# server.arr-vs-server.ts rendering parity. CLI-surface rendering remains
# covered per-program by parity-test.sh.
#
# good/ and bad/ are the live corpus (what tests/type-check/main.arr runs).
# should/ and should-not/ are aspirational known-gap markers that NO harness
# executes -- included here deliberately: parity only asserts the two
# compilers AGREE on today's behavior, and these files are exactly where
# behavior will change if the type checker improves. (One,
# should-not/methods-contested-extension.arr, is bit-rotted to a parse
# error -- old object-literal method shorthand; both compilers must still
# reject it identically.)
#
# Run from the pyret-lang root (lang/):
#   bash src/ts-compiler/tests/type-check-parity.sh          (or `make ts-type-check-parity`)

set -u
cd "$(dirname "$0")/../../.."   # lang/

CORPUS=(tests/type-check/good tests/type-check/bad
        tests/type-check/should tests/type-check/should-not)
WORK=build/ts-compiler/type-check-parity
DRIVER=src/ts-compiler/tests/parity-serve-driver.js

rm -rf "$WORK/out"
mkdir -p "$WORK/out"
. src/ts-compiler/tests/parity-serve-lib.sh

shopt -s nullglob
programs=()
for d in "${CORPUS[@]}"; do programs+=("$d"/*.arr); done
if [ "${#programs[@]}" -eq 0 ]; then
  echo "No corpus programs found under tests/type-check/"
  exit 1
fi
printf '%s\n' "${programs[@]}" > "$WORK/programs.txt"

start_servers

# One sequential driver per compiler, the two running concurrently.
node "$DRIVER" "$ARR_SOCK" "$WORK/programs.txt" "$WORK/out" -arr --type-check &
DRIVE_ARR=$!
node "$DRIVER" "$TS_SOCK" "$WORK/programs.txt" "$WORK/out" -ts --type-check &
DRIVE_TS=$!
wait "$DRIVE_ARR" || { echo "FAIL: arr driver crashed"; exit 1; }
wait "$DRIVE_TS" || { echo "FAIL: ts driver crashed"; exit 1; }

stop_servers
trap - EXIT

pass=0
fail=0
failed_programs=()

for prog in "${programs[@]}"; do
  base=$(basename "$prog" .arr)
  stat_a=$(grep "^$base " "$WORK/out/results-arr.txt" | head -1 | awk '{print $2}')
  stat_t=$(grep "^$base " "$WORK/out/results-ts.txt" | head -1 | awk '{print $2}')

  if [ "$stat_a" != "$stat_t" ] || [ "$stat_a" = "ERR" ] || [ -z "$stat_a" ]; then
    echo "FAIL $base: statuses differ or errored (arr=${stat_a:-missing} ts=${stat_t:-missing})"
    echo "  arr tail: $(tail -2 "$WORK/out/$base-arr.out" 2>/dev/null | head -c 300)"
    echo "  ts  tail: $(tail -2 "$WORK/out/$base-ts.out" 2>/dev/null | head -c 300)"
    fail=$((fail+1)); failed_programs+=("$prog"); continue
  fi

  # Existential-variable labels (?-N) are numbered by solve-loop iteration
  # order, which differs between the compilers (Pyret list-set vs TS Map) and
  # was deliberately left divergent -- see the "Solve-loop iteration order"
  # entry in port-review-nonmechanical.md (a port-era review note; it lives in
  # the ts-port-archive tag, not in this tree): gensym order in messages only,
  # nothing type-checks differently. Canonicalize by order of first
  # appearance in each output; this still fails if the two outputs use
  # DIFFERENT equality patterns among their labels (e.g. `?-1 ... ?-1` in one
  # vs `?-1 ... ?-2` in the other), so real unification differences surface.
  perl -i -pe 's/\?-(\d+)/"?-" . ($seen{$1} \/\/= ++$n)/ge' "$WORK/out/$base-arr.out"
  perl -i -pe 's/\?-(\d+)/"?-" . ($seen{$1} \/\/= ++$n)/ge' "$WORK/out/$base-ts.out"

  if diff -u "$WORK/out/$base-arr.out" "$WORK/out/$base-ts.out" > "$WORK/out/$base.diff"; then
    rm -f "$WORK/out/$base.diff"
    pass=$((pass+1))
  else
    echo "FAIL $base: diagnostics differ (see $WORK/out/$base.diff)"
    fail=$((fail+1)); failed_programs+=("$prog")
  fi
done

echo
echo "type-check parity: $pass passed, $fail failed (of ${#programs[@]})"
if [ "$fail" -ne 0 ]; then
  echo "failed:"
  printf '  %s\n' "${failed_programs[@]}"
  exit 1
fi
