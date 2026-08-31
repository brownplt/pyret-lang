#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

tarball="$PWD/$(npm pack --silent | tail -n 1)"
smokedir=$(mktemp -d)
trap 'rm -rf "$smokedir" "$tarball"' EXIT

cd "$smokedir"
npm init -y > /dev/null
npm install --no-audit --no-fund "$tarball" > /dev/null

cat > smoke.arr <<'ARR'
check "smoke":
  1 + 1 is 2
end
ARR

run_backend() {
  local backend="$1"
  local sock="$smokedir/comm-$backend.sock"
  local out="$smokedir/out-$backend.log"
  local status
  set +e
  node node_modules/pyret-npm/pyret.js --backend "$backend" --port "$sock" smoke.arr > "$out" 2>&1
  status=$?
  set -e
  node node_modules/pyret-npm/pyret.js --backend "$backend" --port "$sock" -s > /dev/null 2>&1 || true
  if [ "$status" -ne 0 ] || ! grep -q "Looks shipshape" "$out"; then
    echo "packaged cli failed for backend $backend (exit $status):"
    cat "$out"
    exit 1
  fi
  echo "packaged cli: backend $backend is shipshape"
}

run_backend pyret
if [ -e node_modules/pyret-npm/pyret-lang/build/ts-compiler/pyret.js ]; then
  run_backend ts
else
  echo "packaged cli: ts backend not in package, skipped"
fi
