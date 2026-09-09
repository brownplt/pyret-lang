#!/usr/bin/env bash
# Both module finders must handle the same set of import protocols.
# Run from lang/: bash src/ts-compiler/tests/loader-protocol-parity.sh

set -u
cd "$(dirname "$0")/../../.."

ARR=src/arr/compiler/cli-module-loader.arr
TS=src/ts-compiler/src/cli-module-loader.ts

arr_protocols=$(grep -oE 'protocol == "[a-z-]+"' "$ARR" | grep -oE '"[a-z-]+"' | tr -d '"' | sort -u)
ts_protocols=$(grep -oE 'protocol === "[a-z-]+"' "$TS" | grep -oE '"[a-z-]+"' | tr -d '"' | sort -u)

if [ -z "$arr_protocols" ] || [ -z "$ts_protocols" ]; then
  echo "loader-protocol-parity: could not extract protocols" >&2
  exit 2
fi

if diff <(echo "$arr_protocols") <(echo "$ts_protocols") > /dev/null; then
  echo "loader-protocol-parity: ok ($(echo "$arr_protocols" | wc -l | tr -d ' ') protocols)"
  exit 0
fi

echo "loader-protocol-parity: import protocols differ between $ARR (<) and $TS (>)" >&2
diff <(echo "$arr_protocols") <(echo "$ts_protocols") >&2
exit 1
