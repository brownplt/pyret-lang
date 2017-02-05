#!/usr/bin/env bash
if [ "$#" -ne 2 ]; then
    >&2 echo "Usage: $0 base-url pyret-dir"
    exit 1
fi

SCRIPT_DIR="$(dirname $0)"
BASE_URL="$1"
PYRET_DIR="$2"
PYRET_RELDIR="$(${SCRIPT_DIR}/calc-relpath.sh ${BASE_URL} ${PYRET_DIR})"

cat <<EOF
{
  "baseUrl": "${BASE_URL}",
  "optimize": "none",
  "paths": {
      "pyret-base": "${PYRET_RELDIR}/build/phaseA",
      "jglr": "${PYRET_RELDIR}/lib/jglr",
      "crypto": "empty:",
      "path": "empty:",
      "q": "${PYRET_RELDIR}/node_modules/q/q",
      "s-expression": "${PYRET_RELDIR}/node_modules/s-expression/index",
      "fs": "empty:",
      "requirejs": "${PYRET_RELDIR}/node_modules/requirejs/require",
      "seedrandom": "${PYRET_RELDIR}/node_modules/seedrandom/seedrandom"
    }
}
EOF
