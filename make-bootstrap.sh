#!/bin/bash

bash make-standalone.sh build/phase1/main-wrapper.js \
                        src/arr/pyret/compilerA/pyret.arr \
                        build/pyretA.jarr \
                        build/bootstrapA

bash make-standalone.sh build/pyretA.jarr \
                        src/arr/pyret/compilerA/pyret.arr \
                        build/pyretB.jarr \
                        build/bootstrapB

bash make-standalone.sh build/pyretB.jarr \
                        src/arr/pyret/compilerA/pyret.arr \
                        build/pyretC.jarr \
                        build/bootstrapC

