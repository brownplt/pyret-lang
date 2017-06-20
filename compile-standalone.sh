#!/usr/bin/env bash

if [ "$#" -lt 1 ]; then
    >&2 echo "Usage: $0 prog.arr"
    exit 1
fi

FILE=""
COMPILED_DIR=""
PHASE="A"
while [ "$#" -gt 0 ]; do
    case "$1" in
        --phase|-p)
            if [ "$#" -lt 2 ]; then
                >&2 echo "Missing phase after $1"
                exit 1
            fi
            PHASE="$2"
            shift
            shift
            ;;
        --compiled|-c)
            if [ "$#" -lt 2 ]; then
                >&2 echo "Missing directory after $1"
                exit 1
            fi
            COMPILED_DIR="$2"
            shift
            shift
            ;;
        *)
            if [ ! -z "$FILE" ]; then
                >&2 echo "You may only specify one file."
                exit 1
            fi
            FILE="$1"
            shift
            ;;
    esac
done

if [ -z "$COMPILED_DIR" ]; then
    COMPILED_DIR="$(dirname $0)/standalone${PHASE}-compiled"
fi

node -max-old-space-size=8192 \
     "build/phase${PHASE}/pyret.jarr" \
     --require-config src/scripts/standalone-configA.json \
     --builtin-js-dir src/js/trove \
     --builtin-arr-dir src/arr/trove \
     --compiled-dir "$COMPILED_DIR" \
     --outfile "${FILE%%.arr}.jarr" \
     --build-runnable "$FILE"
