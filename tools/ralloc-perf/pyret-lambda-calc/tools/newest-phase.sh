#!/usr/bin/env bash
if [ "$#" -lt 2 ]; then
    >&2 echo "Usage: $0 build-dir phase [phases ...]"
    exit 1
fi

BUILD_DIR="$1"
shift
NEWEST="$1"
shift

for phase in $@; do
    if [ "$BUILD_DIR/compiled-$NEWEST/built" -ot "$BUILD_DIR/compiled-$phase/built" ]; then
        NEWEST="$phase"
    fi
done
echo "$NEWEST"
