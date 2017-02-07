#!/usr/bin/env bash
set -e

SCRIPT_DIR=$(dirname $0)
(cd ${SCRIPT_DIR} && npm install)

pyret-ar-print() {
    ${SCRIPT_DIR}/node_modules/.bin/pyret-ar-print $@
}

test-lambda-calc() {
    (cd ${SCRIPT_DIR}/pyret-lambda-calc && make phase0)
    PHASE0_JARR=${SCRIPT_DIR}/pyret-lambda-calc/build/run-lc-jarr.phase0
    PHASEA_JARR=${SCRIPT_DIR}/pyret-lambda-calc/build/run-lc-jarr.phaseA
    cp ${SCRIPT_DIR}/pyret-lambda-calc/build/run-lc.jarr ${PHASE0_JARR}
    (cd ${SCRIPT_DIR}/pyret-lambda-calc && make phaseA)
    cp ${SCRIPT_DIR}/pyret-lambda-calc/build/run-lc.jarr ${PHASEA_JARR}
    echo "Lambda Calculus Repository Results (Phase 0 -> Phase A)"
    pyret-ar-print --diff ${PHASE0_JARR} ${PHASEA_JARR}
}

test-pyret-compiler() {
    (cd ${SCRIPT_DIR}/../.. && make phaseB)
    PHASEA_JARR=${SCRIPT_DIR}/../../build/phaseA/pyret.jarr
    PHASEB_JARR=${SCRIPT_DIR}/../../build/phaseB/pyret.jarr
    echo "Pyret Compiler Results (Phase A -> Phase B)"
    pyret-ar-print --diff ${PHASEA_JARR} ${PHASEB_JARR}
}

test-lambda-calc
test-pyret-compiler
