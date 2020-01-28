#!/bin/bash


OUTDIR=${1:-../outputs}
shift
PROG=${*:-../tests.native}

echo "outdir=<$OUTDIR> prog=<$PROG>"

TESTDIR=${1:-../tests/benchmarks/}
INS="array fpice mochi termination" # 
DOMAIN="Oct"

OUTPRE="out"

echo "Build program before testing..."
cd ../ && ./build.sh
cd scripts

echo "Starting benchmarks testing..."
for dir in ${INS}; do
    for f in `find ${TESTDIR}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
        echo "${PROG} -file ${TESTDIR}${dir}/${f} -int -domain ${DOMAIN}"
        ts=$(gdate +%s%N)
        ${PROG} -file ${TESTDIR}/${dir}/${f} -int -domain ${DOMAIN} > ${OUTDIR}/${dir}/${OUTPRE}_${f}
        tt=$((($(gdate +%s%N) - $ts)/1000000))
        echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
    done
done

echo "Gnerate table results..."
python3 table.py  
