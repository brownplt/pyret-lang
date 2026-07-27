#!/usr/bin/env bash
#
# run-all.sh -- run code.pyret.org's editor assertions against all three
# environments, via the single Playwright runner (run.js):
#
#   cpo    -- the reference: reproduces upstream's outcomes on /editor
#   embed  -- the embed API's embedded instance
#   vscode -- the pyret-parley.cpo webview (headless VS Code for the Web)
#
# Strictly additive: only reads code.pyret.org / vscode, writes under results/.
#
# Prereqs (once):
#   - code.pyret.org built (build/web/js/cpo-main.jarr) + its npm deps
#   - vscode extension built: (cd vscode && ln -sf ../code.pyret.org/build build
#       && npm install && npm run compile)
#   - this harness's deps: (cd browser-test && npm install)
#   - Chrome (GOOGLE_CHROME_BINARY, default /bin/google-chrome)
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
CPO="$(cd "$HERE/.." && pwd)/code.pyret.org"
RESULTS="$HERE/results"
mkdir -p "$RESULTS"

export BASE_URL="${BASE_URL:-http://localhost:4999}"
export GOOGLE_CHROME_BINARY="${GOOGLE_CHROME_BINARY:-/bin/google-chrome}"
# env expected by the CPO server (cpo + embed environments load /editor from it)
export PORT="${PORT:-4999}"
export PYRET="${PYRET:-http://localhost:4999/js/cpo-main.jarr}"
export POSTMESSAGE_ORIGIN="${POSTMESSAGE_ORIGIN:-*}"
export NODE_ENV="${NODE_ENV:-development}"
export SESSION_SECRET="${SESSION_SECRET:-not-so-secret}"
export URL_FILE_MODE="${URL_FILE_MODE:-all-remote}"
export IMAGE_PROXY_BYPASS="${IMAGE_PROXY_BYPASS:-true}"
export SHARED_FETCH_SERVER="${SHARED_FETCH_SERVER:-https://code.pyret.org}"

# Start the CPO server if /editor isn't already being served (cpo + embed need it).
if ! curl -fs -o /dev/null "$BASE_URL/editor"; then
  echo "Starting CPO server..."
  ( cd "$CPO" && node src/run.js > "$RESULTS/cpo-server.log" 2>&1 & )
  for _ in $(seq 1 30); do curl -fs -o /dev/null "$BASE_URL/editor" && break; sleep 1; done
fi
curl -fs -o /dev/null "$BASE_URL/editor" || { echo "CPO server not reachable at $BASE_URL"; exit 1; }

rc=0
for ENVNAME in cpo embed vscode; do
  echo "=== $ENVNAME ==="
  node "$HERE/run.js" --env="$ENVNAME" "$@" | tee "$RESULTS/$ENVNAME-full.txt"
  test "${PIPESTATUS[0]}" -eq 0 || rc=1
done
echo "Done. See $RESULTS/. (overall rc=$rc)"
exit $rc
