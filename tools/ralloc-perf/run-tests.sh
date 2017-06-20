#!/usr/bin/env bash
set -e

SCRIPT_DIR=$(dirname $0)
(cd ${SCRIPT_DIR} && npm install)

pyret-ar-print() {
    node ${SCRIPT_DIR}/node_modules/.bin/pyret-ar-print $@
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
#  node ~/junk/js/ar-print/main.js -l <(comm -13 <(cd pyret-lambda-calc/build/compiled-phase0/ && ls *-module.js) <(cd seam-carving/build/compiled-phase0/&& ls *-module.js) | sed -e 's/^/seam-carving\/build\/compiled-phase0\//')

get-unique-files() {
    BASE=$1
    PHASE=$2
    PLC_DIR=${SCRIPT_DIR}/pyret-lambda-calc/build/compiled-${PHASE}
    NEW_DIR=${BASE}/build/compiled-${PHASE}
    comm -13 <(cd ${PLC_DIR} && ls *-module.js) <(cd ${NEW_DIR} && ls *-module.js) | \
        sed -e "s#^#${BASE}/build/compiled-${PHASE}/#"
}

test-seam-carving() {
    if [ ! -e "${SCRIPT_DIR}/seam-carving/code.pyret.org" ]; then
        >&2 echo "[WARNING] Skipping seam-carving test. To run, link $(cd ${SCRIPT_DIR} && pwd)/seam-carving/code.pyret.org"
        >&2 echo "          to the root directory of the code.pyret.org repository."
        return 0
    fi
    (cd ${SCRIPT_DIR}/seam-carving && make phase0)
    PHASE0_JARR=${SCRIPT_DIR}/seam-carving/build/carve-seams-jarr.phase0
    PHASEA_JARR=${SCRIPT_DIR}/seam-carving/build/carve-seams-jarr.phaseA
    (cd ${SCRIPT_DIR}/seam-carving && make phaseA)
    echo "Seam Carving Results (Phase 0 -> Phase A)"
    echo "Note: Only modules not in pyret-lambda-calc are counted."
    pyret-ar-print -l \
                   --diff \
                   <(get-unique-files ${SCRIPT_DIR}/seam-carving phase0) \
                   <(get-unique-files ${SCRIPT_DIR}/seam-carving phaseA)
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
test-seam-carving
