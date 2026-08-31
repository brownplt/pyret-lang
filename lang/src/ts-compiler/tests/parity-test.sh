#!/usr/bin/env bash
# Parity test: compile each test program with BOTH the Pyret-hosted
# compiler (build/phaseA/pyret.jarr) and the TypeScript compiler
# (build/ts-compiler/pyret.js), using the same options, require the
# standalones to be byte-identical, run both, and compare stdout +
# exit codes.
#
# Run from the pyret-lang root (lang/): bash src/ts-compiler/tests/parity-test.sh

set -u
cd "$(dirname "$0")/../../.."   # lang/

NODE="node --max-old-space-size=8192"
PYRET_ARR=build/phaseA/pyret.jarr
PYRET_TS=build/ts-compiler/pyret.js
PROGRAMS_DIR=src/ts-compiler/tests/programs
WORK=build/ts-compiler/parity
COMMON_OPTS=(--builtin-js-dir src/js/trove/
             --builtin-arr-dir src/arr/trove/
             --require-config src/scripts/standalone-configA.json
             --deps-file build/phaseA/bundled-node-compile-deps.js
             -no-display-progress)

mkdir -p "$WORK"
pass=0
fail=0
failed_programs=()

# Per-program extra options live in an optional "<prog>.options" file
# (one option per line), so the same matrix runs through both compilers.
run_one() {
  local prog="$1"
  local base
  base=$(basename "$prog" .arr)
  local extra_opts=()
  if [ -f "$PROGRAMS_DIR/$base.options" ]; then
    while IFS= read -r line; do
      [ -n "$line" ] && extra_opts+=($line)
    done < "$PROGRAMS_DIR/$base.options"
  fi

  local dir_a="$WORK/$base-arr" dir_t="$WORK/$base-ts"
  mkdir -p "$dir_a" "$dir_t"

  $NODE $PYRET_ARR "${COMMON_OPTS[@]}" ${extra_opts[@]+"${extra_opts[@]}"} \
        --compiled-dir "$dir_a/compiled" \
        --build-runnable "$prog" --outfile "$dir_a/$base.jarr" \
        > "$dir_a/compile.out" 2>&1
  local cstat_a=$?
  $NODE $PYRET_TS "${COMMON_OPTS[@]}" ${extra_opts[@]+"${extra_opts[@]}"} \
        --compiled-dir "$dir_t/compiled" \
        --build-runnable "$prog" --outfile "$dir_t/$base.jarr" \
        > "$dir_t/compile.out" 2>&1
  local cstat_t=$?

  if [ "$cstat_a" -ne "$cstat_t" ]; then
    echo "FAIL $base: compile exit codes differ (arr=$cstat_a ts=$cstat_t)"
    echo "  arr compile tail: $(tail -2 "$dir_a/compile.out" | head -c 300)"
    echo "  ts compile tail: $(tail -2 "$dir_t/compile.out" | head -c 300)"
    return 1
  fi

  # If compilation failed in both, compare the error output (modulo the
  # progress lines, which COMMON_OPTS already suppress). The Pyret-hosted
  # compiler appends a "Pyret stack:" trailer pointing into its own
  # compiler sources; that is compiler-internal and stripped before diffing.
  if [ "$cstat_a" -ne 0 ]; then
    sed -i '/^Pyret stack:/,$d' "$dir_a/compile.out"
    sed -i '/^Pyret stack:/,$d' "$dir_t/compile.out"
    if diff -u "$dir_a/compile.out" "$dir_t/compile.out" > "$WORK/$base.compile.diff"; then
      return 0
    else
      echo "FAIL $base: compile error output differs (see $WORK/$base.compile.diff)"
      return 1
    fi
  fi

  if ! cmp -s "$dir_a/$base.jarr" "$dir_t/$base.jarr"; then
    echo "FAIL $base: standalone bytes differ (cmp $dir_a/$base.jarr $dir_t/$base.jarr)"
    return 1
  fi

  $NODE "$dir_a/$base.jarr" > "$dir_a/run.out" 2>&1
  local rstat_a=$?
  $NODE "$dir_t/$base.jarr" > "$dir_t/run.out" 2>&1
  local rstat_t=$?

  if [ "$rstat_a" -ne "$rstat_t" ]; then
    echo "FAIL $base: run exit codes differ (arr=$rstat_a ts=$rstat_t)"
    return 1
  fi
  if ! diff -u "$dir_a/run.out" "$dir_t/run.out" > "$WORK/$base.run.diff"; then
    echo "FAIL $base: run output differs (see $WORK/$base.run.diff)"
    return 1
  fi
  return 0
}

shopt -s nullglob
programs=("$PROGRAMS_DIR"/*.arr)
if [ "${#programs[@]}" -eq 0 ]; then
  echo "No test programs found in $PROGRAMS_DIR"
  exit 1
fi

for prog in "${programs[@]}"; do
  if run_one "$prog"; then
    echo "ok   $(basename "$prog")"
    pass=$((pass+1))
  else
    fail=$((fail+1))
    failed_programs+=("$(basename "$prog")")
  fi
done

echo
echo "parity: $pass passed, $fail failed"
if [ "$fail" -ne 0 ]; then
  echo "failed: ${failed_programs[*]}"
  exit 1
fi
