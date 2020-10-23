#!/bin/bash

# ./r_type_runit.sh -set call/unv
OUTDIR="../outputs/r_type"
PROG="../comp_bin/r_type_macos"
PROGNAME="r_type"
SET=$2
SOLVER="z3 -in"
SOLVERPRINT="\"z3 -in\""
DATE="gdate"
if [[ "$OSTYPE" != "darwin"* ]]; then
    DATE="date"
    PROG="../comp_bin/r_type_ubuntu"
fi
echo "outdir=<$OUTDIR> prog=<$PROG>"

shift
shift

if [ $# -eq 1 ]; then
    if [ $1 = "-hoice" ]; then
        echo "Use $1";
        SOLVER="hoice";
        SOLVERPRINT="hoice";
    else
        echo "Use z3";
    fi
fi

DIRS=" DRIFT DOrder r_type"
INS=" array list first high negative termination " # 
timeout="300" # timeout set for 5 minutes
OUTPRE="out"

for dir in ${INS}; do
    if [ ! -d ${OUTDIR}/${dir} ]; then
        mkdir -p ${OUTDIR}/${dir}
    fi
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
                echo "${PROG} --solver ${SOLVERPRINT} ${TESTDIR}/${hdir}/${dir}/${f}"
                ts=$(${DATE} +%s%N)
                timeout ${timeout} ${PROG} --solver "${SOLVER}" ${TESTDIR}/${hdir}/${dir}/${f} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                tt=$((($(${DATE} +%s%N) - $ts)/1000000))
                st=$(($timeout*1000))
                if [[ $tt -gt $st ]]; then
                    echo "Time: timeout" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                else
                    echo "Time: $tt" >> ${OUTDIR}/${dir}/${OUTPRE}_${f}
                fi
            done
        fi
    done
done

csv_name="res-rtype"
if [[ $SOLVER == "hoice" ]]; then
    csv_name="${csv_name}-hoice"
else
    csv_name="${csv_name}-z3"
fi

echo "Generate ${csv_name} table results..."
python3 r_type_table.py -csv ${csv_name}

mv ../${csv_name}.csv ../result/comp_tools/unv