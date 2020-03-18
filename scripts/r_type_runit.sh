#!/bin/bash


OUTDIR="../outputs/r_type"
PROG="../comp_bin/r_type_macos"

echo "outdir=<$OUTDIR> prog=<$PROG>"

TESTDIR="../tests_eq/benchmarks/"
DIRS=" DART_IT DOrder r_type"
INS=" array first high negative universe" #  termination
timeout="5"
OUTPRE="out"

echo "Starting benchmarks testing..."
for hdir in ${DIRS}; do
    for dir in ${INS}; do
        if [ -d "${TESTDIR}${hdir}/${dir}" ]; then
            for f in `find ${TESTDIR}${hdir}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
                echo "${PROG} --solver \"z3 -in\" ${TESTDIR}${hdir}/${dir}/${f}"
                ts=$(gdate +%s%N)
                timeout ${timeout} ${PROG} --solver "z3 -in" ${TESTDIR}${hdir}/${dir}/${f} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                if [[ $? -ne 0 ]]; then
                    echo "Time: timeout" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                else
                    tt=$((($(gdate +%s%N) - $ts)/1000000))
                    echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                fi
            done
        fi
    done
done

echo "Gnerate table results..."
python3 r_type_table.py  
