# Shared server management for the serve-mode parity harnesses
# (type-check-parity.sh, wf-parity.sh). Source from lang/ with:
#   WORK=<workdir> ; . src/ts-compiler/tests/parity-serve-lib.sh
# Provides start_servers / stop_servers; sets ARR_SOCK / TS_SOCK.
#
# Both compilers expose the same -serve protocol the npm client drives
# (server.arr / server.ts); booting each ONCE amortizes the .arr compiler's
# ~2s CLI startup across the whole corpus.

SMOKE=src/ts-compiler/tests/serve-smoke.js
ARR_SOCK="$PWD/$WORK/arr.sock"
TS_SOCK="$PWD/$WORK/ts.sock"
ARR_SERVER_PID=
TS_SERVER_PID=

start_servers() {
  node --max-old-space-size=8192 build/phaseA/pyret.jarr \
    -serve --port "$ARR_SOCK" > "$WORK/arr-server.log" 2>&1 &
  ARR_SERVER_PID=$!
  PYRET_TS_NO_RESPAWN=1 node --stack-size=8192 --max-old-space-size=8192 \
    build/ts-compiler/pyret.js -serve --port "$TS_SOCK" > "$WORK/ts-server.log" 2>&1 &
  TS_SERVER_PID=$!
  trap stop_servers EXIT
  for _ in $(seq 1 120); do
    [ -S "$ARR_SOCK" ] && [ -S "$TS_SOCK" ] && return 0
    if ! kill -0 "$ARR_SERVER_PID" 2>/dev/null || ! kill -0 "$TS_SERVER_PID" 2>/dev/null; then
      echo "FAIL: a compile server exited during startup; log tails:"
      tail -3 "$WORK/arr-server.log" "$WORK/ts-server.log"
      exit 1
    fi
    sleep 0.5
  done
  echo "FAIL: server sockets never appeared"
  exit 1
}

stop_servers() {
  node "$SMOKE" "$ARR_SOCK" --shutdown > /dev/null 2>&1
  node "$SMOKE" "$TS_SOCK" --shutdown > /dev/null 2>&1
  # Fallback for abnormal exits; harmless if shutdown already worked.
  kill "$ARR_SERVER_PID" "$TS_SERVER_PID" 2>/dev/null
  wait "$ARR_SERVER_PID" "$TS_SERVER_PID" 2>/dev/null
}
