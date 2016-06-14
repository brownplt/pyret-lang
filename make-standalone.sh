#!/bin/bash

COMPILER=$1
TARGET=$2
OUT=$3
WHERE=$4
#COMPILER=src/arr/compilerA/pyret.jarr

node $COMPILER \
              --builtin-js-dir src/js/trove/ \
              --builtin-arr-dir src/arr/trove/ \
              --compiled-dir $WHERE/compiled/ \
              -no-check-mode \
              --build-standalone $TARGET \
              > $WHERE/program.js

# keep only the initial call to define, with its dependency list...
sed -e '/]/,$d' $WHERE/program.js > $WHERE/program-require.js
# and close it off...
cat <<END >> $WHERE/program-require.js
  ]);
requirejs(["js/runtime"]);
END
# then rename the call from define to requirejs.
sed -i -- 's/define/requirejs/' $WHERE/program-require.js

# Use requirejs to process that minimal file
node node_modules/requirejs/bin/r.js -o src/scripts/require-build.js \
  baseUrl=$WHERE name=program-require \
  out=$TARGET-require.js

# Construct the working standalone by initializing requirejs,
cat <<END > $OUT
requirejs = require("requirejs");
define = requirejs.define;
END
# including the native requires,
cat $TARGET-require.js >> $OUT
# the program itself,
cat $WHERE/program.js | sed -e 's/define(\[/define("program", \[/' >> $OUT
# and the handalone driver
cat src/js/handalone.js >> $OUT
