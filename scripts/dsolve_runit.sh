#!/bin/bash

PROGDIR="../../dsolve" # Change directory here if you built it in another place
DRIFTDIR="../drift"
echo "Assume dsolve is built under relative path ../../dsolve"
cp -a ../tests/benchmarks ${PROGDIR}/tests
cp dsolve_table.py ${PROGDIR}
pushd ${PROGDIR}
# ./dsolve_runit.sh -set call/unv
OUTDIR="outputs/dsolve"
PROG="./dsolve.py"
ARRAYSET="DRIFT"
SET=$2
echo "outdir=<$OUTDIR> prog=<$PROG>"

DIRS=" DRIFT DOrder r_type" #
INS=" array list first high negative termination " # 
timeout="300"
OUTPRE="out"

if [ $SET = "call" ]; then
    TESTDIR="tests/benchmarks_call"
else
    TESTDIR="tests/benchmarks"
fi

for dir in ${INS}; do
    if [ ! -d ${OUTDIR}/${dir} ] 
    then
        mkdir -p ${OUTDIR}/${dir}
    fi
    rm -f ${OUTDIR}/${dir}/* # Remove all outputs from last tests
done

echo "Starting benchmarks testing..."
for hdir in ${DIRS}; do
    for dir in ${INS}; do
        if [ -d "${TESTDIR}/${hdir}/${dir}" ]; then
            if [ ${dir} = "array" ] && [ ${hdir} != ${ARRAYSET} ]; then
                continue
            fi 
            for f in `find ${TESTDIR}/${hdir}/${dir} ! -iname "*annot*" -and ! -iname "*quals*" -and \
                ! -iname ".DS_Store" -and ! -iname "*.json" -and ! -iname "*.status" -and ! -iname "*.horsat_out" \
                -and ! -iname "*.hors" -and ! -iname "*.smt2" \
                -type f -execdir echo '{}' ';'`; do
                b=${f#*./}
                ts=$(date +%s%N)
                    echo "${PROG} -v 0 ${TESTDIR}/${hdir}/${dir}/${b}"
                    timeout ${timeout} ${PROG} -v 0 ${TESTDIR}/${hdir}/${dir}/${b} &> ${OUTDIR}/${dir}/${OUTPRE}_${b}
                if [[ $? -eq 124 ]]; then
                    echo "Time: timeout" >> ${OUTDIR}/${dir}/${OUTPRE}_${b}
                else
                    tt=$((($(date +%s%N) - $ts)/1000000))
                    echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${b}
                fi
            done
        fi
    done
done

echo "Gnerate dsolve table results..."
python3 dsolve_table.py  

mv res-dsolve.csv ${DRIFTDIR}/result/comp_tools/unv

popd
