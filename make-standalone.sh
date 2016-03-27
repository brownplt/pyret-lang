#!/bin/bash

TARGET=tests/pyret/standalone/importer

node build/phase1/main-wrapper.js \
              --builtin-js-dir src/js/troveA/ \
              --builtin-arr-dir src/arr/troveA/ \
              -no-check-mode \
              --build-standalone $TARGET.arr \
              > build/phaseA/program.js

# keep only the initial call to define, with its dependency list...
sed -e '/]/,$d' build/phaseA/program.js > build/phaseA/program-require.js
# and close it off...
cat <<END >> build/phaseA/program-require.js
  ]);
requirejs(["runtime"]);
END
# then rename the call from define to requirejs.
sed -i -- 's/define/requirejs/' build/phaseA/program-require.js

# Use requirejs to process that minimal file
node node_modules/requirejs/bin/r.js -o src/scripts/require-build.js \
  baseUrl=build/phaseA/ name=program-require \
  out=$TARGET-require.js

# Construct the working standalone by initializing requirejs,
cat <<END > $TARGET.jarr
requirejs = require("requirejs");
define = requirejs.define;
END
# including the native requires,
cat $TARGET-require.js >> $TARGET.jarr
# the program itself,
cat build/phaseA/program.js | sed -e 's/define(\[/define("program", \[/' >> $TARGET.jarr
# and the handalone driver
cat build/phaseA/handalone.js >> $TARGET.jarr
