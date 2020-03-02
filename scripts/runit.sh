#!/bin/bash


OUTDIR=${1:-../outputs}
shift
PROG=${*:-../tests.native}

echo "outdir=<$OUTDIR> prog=<$PROG>"

TESTDIR=${1:-../tests/benchmarks/}
DIRS=" DART_IT DOrder r_type"
INS=" first high negative array" # termination  
DOMAIN="Oct"

OUTPRE="out"

echo "Build program before testing..."
cd ../ && ./build.sh
cd scripts

echo "Starting benchmarks testing..."
for hdir in ${DIRS}; do
    for dir in ${INS}; do
        if [ -d "${TESTDIR}${hdir}/${dir}" ]; then
            for f in `find ${TESTDIR}${hdir}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
                echo "${PROG} -file ${TESTDIR}${hdir}/${dir}/${f} -int -domain ${DOMAIN}"
                ts=$(gdate +%s%N)
                ${PROG} -file ${TESTDIR}${hdir}/${dir}/${f} -int -domain ${DOMAIN} > ${OUTDIR}/${dir}/${OUTPRE}_${f}
                tt=$((($(gdate +%s%N) - $ts)/1000000))
                echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
            done
        fi
    done
done

echo "Gnerate table results..."
python3 table.py  
