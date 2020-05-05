#!/bin/bash

# ./runit.sh -set call/unv -domain Oct [-nar | -dwid 0]
# Domain for benchmarks: Polka_st, Oct, Ppl_st

OUTDIR="../outputs/DART_IT"
PROG="../tests.native"
PROGNAME="DART_IT"
SET=$2
DOMAIN=$4
if [ $# -gt 1 ]; then
    SET=$2
    DOMAIN=$4
else
    echo "ERROR!!! The command should be:"
    echo "./runit.sh -domain <domain_name> [-dwid <delay_steps> | -nar]"
    exit 0
fi
shift
shift
shift
shift

wid=0
nar=false

if [ $# -eq 1 ] && [ $1 = "-nar" ]; then
    echo "Wid + Nar..."
    wid=0;
    nar=true;
else 
    if [ $# -eq 2 ] && [ $1 = "-dwid" ]; then
        echo "Delay widening + Nar. Widen steps occur after $2"
        nar=true;
        wid=$2;
    else 
        echo "Default. Standard widening..."
    fi
fi
echo "outdir=<$OUTDIR> prog=<$PROG>"

if [ $SET = "call" ]; then
    TESTDIR="../tests/benchmarks_call"
else
    TESTDIR="../tests/benchmarks"
fi

DIRS=" DART_IT DOrder r_type"
INS=" first high negative array" #   
timeout="120"
OUTPRE="out"
DATE="gdate"
if [[ "$OSTYPE" != "darwin"* ]]; then
    DATE="date"
fi

echo "Build program before testing..."
cd ../ && ./build.sh clean && ./build.sh
cd scripts

for dir in ${INS}; do
    rm -f ${OUTDIR}/${dir}/* # Remove all outputs from last tests
done

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
                echo "${PROG} -file ${TESTDIR}/${hdir}/${dir}/${f} -out 2 -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar}"
                ts=$(${DATE} +%s%N)
                timeout ${timeout} ${PROG} -file ${TESTDIR}/${hdir}/${dir}/${f} -out 2 -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
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
python3 table.py  
