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
mkdir -p pyret-lang/build
cp -r ../lang/build/phaseA pyret-lang/build/
if [ "${PYRET_NPM_TS:-}" = "1" ]; then
  cp -r ../lang/build/ts-compiler pyret-lang/build/
  mkdir -p pyret-lang/src/js/base pyret-lang/lib/jglr
  cp ../lang/src/js/base/js-numbers.js \
     ../lang/src/js/base/type-util.js \
     ../lang/src/js/base/pyret-tokenizer.js \
     pyret-lang/src/js/base/
  cp ../lang/lib/jglr/jglr.js \
     ../lang/lib/jglr/rnglr.js \
     ../lang/lib/jglr/cyclicJSON.js \
     pyret-lang/lib/jglr/
fi
