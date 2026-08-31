#!/usr/bin/env bash
# Compile-server test: start the TS compiler in the npm CLI's server mode and
# drive it through the npm client's protocol (serve-smoke.js speaks it).
#
# This is the coverage for the `pyret --backend ts` server path: the npm
# client spawns `node <pyret.js> -serve --port <socket>` (client-lib.js) and
# sends compile/shutdown commands over ws+unix. Exercises, in order:
#   1. good compile  -> compile-success, outfile exists, AND the produced
#                       standalone runs and prints the expected output
#   2. bad compile   -> compile-failure (server survives a failing job)
#   3. second good compile -> the server accepts a new connection after
#                       closing the previous one (the npm client reconnects
#                       per job), with the now-warm cache
#   4. shutdown      -> the npm client's shutdown command; the server
#                       process must actually exit
#
# Depends on ts-libA: the serve handler injects <pyretDir>/lib-compiled as a
# compiled-read-only dir (server.ts), which is how the npm installation
# ships precompiled builtins.
#
# Run from the pyret-lang root (lang/): bash src/ts-compiler/tests/serve-test.sh

set -u
cd "$(dirname "$0")/../../.."   # lang/

PYRET_TS=build/ts-compiler/pyret.js
WORK=build/ts-compiler/serve-test
SMOKE=src/ts-compiler/tests/serve-smoke.js
SOCK="$PWD/$WORK/comm.sock"

rm -rf "$WORK"
mkdir -p "$WORK"

printf 'print("hello-from-the-serve-test")\n' > "$WORK/good.arr"
printf 'print(tostring(no-such-name-serve-test))\n' > "$WORK/bad.arr"

# PYRET_TS_NO_RESPAWN + explicit --stack-size: run the server as ONE node
# process (pyret.ts otherwise re-execs itself for stack size), so the PID we
# hold is the process the shutdown command must terminate.
PYRET_TS_NO_RESPAWN=1 node --stack-size=8192 --max-old-space-size=8192 \
  "$PYRET_TS" -serve --port "$SOCK" > "$WORK/server.log" 2>&1 &
SERVER_PID=$!
trap 'kill "$SERVER_PID" 2>/dev/null' EXIT

# Wait for the socket to appear (the server unlinks/creates it on listen).
for _ in $(seq 1 60); do
  [ -S "$SOCK" ] && break
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    echo "FAIL: server exited before creating its socket; server.log:"
    tail -5 "$WORK/server.log"
    exit 1
  fi
  sleep 0.5
done
if [ ! -S "$SOCK" ]; then
  echo "FAIL: socket $SOCK never appeared; server.log:"
  tail -5 "$WORK/server.log"
  exit 1
fi

fail=0
step() { echo "--- $1"; }

export SMOKE_COMPILED_DIR="$PWD/$WORK/compiled"

step "good compile"
if node "$SMOKE" "$SOCK" "$WORK/good.arr" "$WORK/good.jarr" > "$WORK/good.out" 2>&1; then
  echo "ok   compile-success"
else
  echo "FAIL: good compile (exit $?); tail:"; tail -3 "$WORK/good.out"; fail=1
fi

step "run the produced standalone"
if out=$(node --max-old-space-size=8192 "$WORK/good.jarr" 2>&1) \
   && [ "$out" = "hello-from-the-serve-test" ]; then
  echo "ok   standalone runs and prints expected output"
else
  echo "FAIL: standalone run; got: $out"; fail=1
fi

step "bad compile reports compile-failure"
node "$SMOKE" "$SOCK" "$WORK/bad.arr" "$WORK/bad.jarr" > "$WORK/bad.out" 2>&1
status=$?
# Exit 1 alone isn't enough -- infrastructure failures also produce
# compile-failure. Require the error the program actually contains.
if [ "$status" -eq 1 ] && grep -q "no-such-name-serve-test" "$WORK/bad.out"; then
  echo "ok   compile-failure with the expected unbound-id error"
else
  echo "FAIL: expected exit 1 + unbound error, got $status; tail:"; tail -3 "$WORK/bad.out"; fail=1
fi

step "second compile on a fresh connection (warm cache)"
rm -f "$WORK/good.jarr"
if node "$SMOKE" "$SOCK" "$WORK/good.arr" "$WORK/good.jarr" > "$WORK/good2.out" 2>&1; then
  echo "ok   server accepted a second job"
else
  echo "FAIL: second compile (exit $?); tail:"; tail -3 "$WORK/good2.out"; fail=1
fi

step "shutdown command stops the server"
if node "$SMOKE" "$SOCK" --shutdown > "$WORK/shutdown.out" 2>&1; then
  for _ in $(seq 1 20); do
    kill -0 "$SERVER_PID" 2>/dev/null || break
    sleep 0.5
  done
  if kill -0 "$SERVER_PID" 2>/dev/null; then
    echo "FAIL: server still running after shutdown command"; fail=1
  else
    echo "ok   server exited"
  fi
else
  echo "FAIL: shutdown send (exit $?); tail:"; tail -3 "$WORK/shutdown.out"; fail=1
fi

echo
if [ "$fail" -eq 0 ]; then
  echo "serve test: all steps passed"
else
  echo "serve test: FAILED (server.log tail below)"
  tail -10 "$WORK/server.log"
fi
exit "$fail"
