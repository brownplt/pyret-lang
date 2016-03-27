#!/bin/bash

node build/phase1/main-wrapper.js --builtin-js-dir src/js/troveA/ \
              --builtin-arr-dir src/arr/troveA/ \
              -no-check-mode \
              --build-standalone tests/pyret/standalone/importer.arr \
              > build/phaseA/program.js

node node_modules/requirejs/bin/r.js -o src/scripts/require-build.js \
  baseUrl=build/phaseA/ name=handalone \
  out=tests/pyret/standalone/importer.arr.js

cat <<END > tests/pyret/standalone/importer.jarr
requirejs = require("requirejs");
define = requirejs.define;
END
  

cat tests/pyret/standalone/importer.arr.js >> tests/pyret/standalone/importer.jarr
