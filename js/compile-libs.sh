#!/bin/bash

for i in builtin-libs/*.arr; do
  js=${i%.arr}
  if [[ $js =~ list|option|error|set ]]; then
    LIB="-library"
  else
    LIB=""
  fi
  echo "Compiling $js"
  raco pyret --no-checks pyret.arr --compile-module-js $i $LIB > $js.js
done
