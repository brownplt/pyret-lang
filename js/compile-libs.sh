#!/bin/bash

for i in builtin-libs/*.arr; do
  js=${i%.arr}
  if [[ $js =~ list|option|error|set ]]; then
    LIB="-library"
  else
    LIB=""
  fi
  echo "Compiling $js"
  target=build/`basename $js`.js
  node standalone-compiler/main-wrapper.js --compile-module-js $i $LIB > $target
done
