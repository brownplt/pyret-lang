#!/bin/bash
set -e

cd "$(dirname "$0")"

(cd ../lang && npm ci && make phaseA libA)

# Optionally also build the TypeScript port of the compiler so the packaged
# CLI supports `pyret --backend ts` (see pyret.js). Opt-in because it is
# strictly additive to the stock backend.
if [ "${PYRET_NPM_TS:-}" = "1" ]; then
  (cd ../lang && make ts-compiler ts-libA)
fi

rm -rf pyret-lang
mkdir -p pyret-lang/build pyret-lang/src/arr pyret-lang/src/js pyret-lang/src/scripts pyret-lang/lib
cp -r ../lang/build/phaseA pyret-lang/build/
cp -r ../lang/src/arr/compiler ../lang/src/arr/trove pyret-lang/src/arr/
cp -r ../lang/src/js/trove ../lang/src/js/base pyret-lang/src/js/
cp ../lang/src/scripts/standalone-configA.json pyret-lang/src/scripts/
cp -r ../lang/lib/jglr pyret-lang/lib/
if [ "${PYRET_NPM_TS:-}" = "1" ]; then
  cp -r ../lang/build/ts-compiler pyret-lang/build/
fi
