#!/bin/bash


OUTDIR="../outputs/DART_IT"
PROG="../tests.native"

wid=0
nar=false

if [ $# -eq 1 ] && [ $1 = "nar" ]; then
    echo "Wid + Nar..."
    wid=0;
    nar=true;
else 
    if [ $# -eq 2 ] && [ $1 = "wid" ]; then
        echo "Delay widening. Widen steps occur after $2"
        wid=$2;
    else 
        echo "Default. Standard widening..."
    fi
fi

echo "outdir=<$OUTDIR> prog=<$PROG>"

TESTDIR="../tests/benchmarks/"
DIRS=" DART_IT DOrder r_type"
INS=" first high negative array universe" # termination  
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
                rm -rf ${OUTDIR}/${dir}/${f}
                echo "${PROG} -file ${TESTDIR}${hdir}/${dir}/${f} -int -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar}"
                ts=$(gdate +%s%N)
                ${PROG} -file ${TESTDIR}${hdir}/${dir}/${f} -int -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar} > ${OUTDIR}/${dir}/${OUTPRE}_${f}
                tt=$((($(gdate +%s%N) - $ts)/1000000))
                echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
            done
        fi
    done
done

echo "Gnerate table results..."
python3 table.py  
