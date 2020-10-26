#!/bin/bash

PROGDIR="../../MoCHi" # Change directory here if you built it in another place

opam switch 4.03.0
OUTDIR="../outputs/MoCHi"
PROG="./${PROGDIR}/mochi.opt"
PROGNAME="DRIFT"
SOLVER="-z3"
echo "outdir=<$OUTDIR> prog=<$PROG>"
if [ $# -gt 1 ]; then
    SET=$2
else
    echo "ERROR!!! The command should be:"
    echo "./mochi_runit.sh -set call/unv [-<solver_name>]"
    exit 0
fi

shift
shift

if [ $# -eq 1 ]; then
    echo "Use $1"
    SOLVER=$1;
fi

if [ $SET = "call" ]; then
    TESTDIR="../tests/benchmarks_call"
else
    TESTDIR="../tests/benchmarks"
fi

DIRS=" DRIFT DOrder r_type"
INS=" array list first high negative termination " # 
timeout="300"
OUTPRE="out"
DATE="gdate"
if [ "$OSTYPE" != "darwin"* ]; then
    DATE="date"
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
            if [[ ${dir} = "array" && ${hdir} != ${PROGNAME} ]]; then
                continue
            fi 
            for f in `find ${TESTDIR}/${hdir}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
                if [ "$OSTYPE" != "darwin"* ]; then
                    f=${f#*./}
                fi
                echo "${PROG} ${SOLVER} -only-result ${TESTDIR}/${hdir}/${dir}/${f}"
                ts=$(${DATE} +%s%N)
                timeout ${timeout} ${PROG} ${SOLVER} -only-result ${TESTDIR}/${hdir}/${dir}/${f} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                if [ $? -ne 0 ]; then
                    echo "Time: timeout" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                else
                    tt=$((($(${DATE} +%s%N) - $ts)/1000000))
                    echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                fi
            done
        fi
    done
done

csv_name="res-mochi"
if [ $SOLVER == "-hoice" ]; then
    csv_name="${csv_name}-hoice"
else
    csv_name="${csv_name}-z3"
fi

echo "Generate ${csv_name} table results..."
python3 mochi_table.py -csv ${csv_name}

mv ${csv_name}.csv ../result/comp_tools/unv