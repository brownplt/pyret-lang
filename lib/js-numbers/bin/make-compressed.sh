#!/bin/bash


# {{{ bindir
# Make this PATH-independent
saveP="$PATH"
PATH="/usr/bin:/bin"

# Remember current directory
saveD=`pwd`

# Find absolute path to this script,
# resolving symbolic references to the end
# (changes the current directory):
D=`dirname "$0"`
F=`basename "$0"`
cd "$D"
while test -L "$F"; do
  P=`readlink "$F"`
  D=`dirname "$P"`
  F=`basename "$P"`
  cd "$D"
done
D=`pwd`

# Restore current directory
cd "$saveD"

bindir="$D"
PATH="$saveP"



ROOT="${bindir}/.."

mkdir -p ${ROOT}/dist

java -jar ${ROOT}/lib/compiler.jar --compilation_level ADVANCED_OPTIMIZATIONS --js ${ROOT}/src/js-numbers.js --js_output_file ${ROOT}/dist/js-numbers.js
