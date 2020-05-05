#!/bin/bash

# ./r_type_runit.sh -set call/unv
OUTDIR="../outputs/r_type"
PROG="../comp_bin/r_type_macos"
PROGNAME="r_type"
SET=$2
echo "outdir=<$OUTDIR> prog=<$PROG>"

DIRS=" DART_IT DOrder r_type"
INS=" array first high negative" #  termination
timeout="20"
OUTPRE="out"
DATE="gdate"
if [[ "$OSTYPE" != "darwin"* ]]; then
    DATE="date"
    PROG="../comp_bin/r_type_ubuntu"
fi

for dir in ${INS}; do
    rm -f ${OUTDIR}/${dir}/* # Remove all outputs from last tests
done

if [ $SET = "call" ]; then
    TESTDIR="../tests/benchmarks_call"
else
    TESTDIR="../tests/benchmarks"
fi

echo "Starting benchmarks testing..."
for hdir in ${DIRS}; do
    for dir in ${INS}; do
        if [ -d "${TESTDIR}/${hdir}/${dir}" ]; then
            if [ ${dir} = "array" ] && [ ${hdir} != ${PROGNAME} ]; then
                continue
            fi 
            for f in `find ${TESTDIR}/${hdir}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
                if [[ "$OSTYPE" != "darwin"* ]]; then
                    f=${f#*./}
                fi
                echo "${PROG} --solver \"z3 -in\" ${TESTDIR}/${hdir}/${dir}/${f}"
                ts=$(${DATE} +%s%N)
                timeout ${timeout} ${PROG} --solver "z3 -in" ${TESTDIR}/${hdir}/${dir}/${f} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                if [[ $? -ne 0 ]]; then
                    echo "Time: timeout" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                else
                    tt=$((($(${DATE} +%s%N) - $ts)/1000000))
                    echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                fi
            done
        fi
    done
done

echo "Gnerate table results..."
python3 r_type_table.py  
