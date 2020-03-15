#!/bin/bash


OUTDIR=${1:-../outputs/r_type}
shift
PROG=${*:-../comp_bin/r_type_macos}

echo "outdir=<$OUTDIR> prog=<$PROG>"

TESTDIR=${1:-../tests/benchmarks/}
DIRS=" DART_IT DOrder r_type"
INS=" first high negative universe" # array termination  
DOMAIN="Oct"

OUTPRE="out"

echo "Starting benchmarks testing..."
for hdir in ${DIRS}; do
    for dir in ${INS}; do
        if [ -d "${TESTDIR}${hdir}/${dir}" ]; then
            for f in `find ${TESTDIR}${hdir}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
                echo "${PROG} --solver \"z3 -in\" ${TESTDIR}${hdir}/${dir}/${f}"
                ts=$(gdate +%s%N)
                ${PROG} --solver "z3 -in" ${TESTDIR}${hdir}/${dir}/${f} > ${OUTDIR}/${dir}/${OUTPRE}_${f}
                tt=$((($(gdate +%s%N) - $ts)/1000000))
                echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
            done
        fi
    done
done

echo "Gnerate table results..."
python3 r_type_table.py  
