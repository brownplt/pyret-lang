#!/bin/bash

to_compile=(\
  "compile.arr"\
  "web-compile.arr"\
  "gensym.arr"\
  "pyret.arr"\
  "js-of-pyret.arr"\
  "anf.arr"\
  "ast-anf.arr"\
  "ast-util.arr"\
  "anf-simple-compile.arr"\
  "compile-structs.arr"\
  "ast-split.arr"\
  "desugar.arr"\
  "js-ast.arr"\
)

for i in ${to_compile[@]}; do
  echo "Compiling $i"
  raco pyret --no-checks pyret.arr --compile-module-js $i > $i.js
done
