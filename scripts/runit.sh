#!/bin/bash

# ./runit.sh -set call/unv -domain Oct [-nar | -dwid 0 | -thold] [-sen]
# Domain for benchmarks: Oct, Polka_st, Polka_ls

OUTDIR="../outputs/DRIFT2"
PROG="../drift2.native"
PROGNAME="DRIFT2"
SET=$2
DOMAIN=$4
if [ $# -gt 1 ]; then
    SET=$2
    DOMAIN=$4
else
    echo "ERROR!!! The command should be:"
    echo "./runit.sh -set call/unv -domain <domain_name> [-dwid <delay_steps> | -nar]"
    exit 0
fi
shift
shift
shift
shift

wid=0
nar=false
thold=false
sen=false

if [[ $# -ge 1 && ($1 = "-sen" || $2 = "-sen" || $3 = "-sen") ]]; then
    echo "Use sensitive"
    sen=true;
fi

if [ $# -ge 1 ] && [ $1 = "-nar" ]; then
    echo "Wid + Nar..."
    wid=0;
    nar=true;
elif [ $# -ge 1 ] && [ $1 = "-thold" ]; then
    echo "Wid with threshold..."
    thold=true;
    nar=false;
    wid=0;
else 
    if [ $# -ge 2 ] && [ $1 = "-dwid" ]; then
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

DIRS=" DRIFT2 DOrder r_type"
INS=" first high negative array list" #   
timeout="300"
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
                echo "${PROG} -file ${TESTDIR}/${hdir}/${dir}/${f} -out 2 -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar} -thold ${thold} -sen ${sen}"
                ts=$(${DATE} +%s%N)
                timeout ${timeout} ${PROG} -file ${TESTDIR}/${hdir}/${dir}/${f} -out 2 -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar} -thold ${thold} -sen ${sen} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
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

csv_name="res"

if [[ $sen == true ]]; then
    csv_name="${csv_name}1-"
else
    csv_name="${csv_name}-"
fi

DOMAIN=$(echo "$DOMAIN" | tr '[:upper:]' '[:lower:]')
# csv_name="${csv_name}${DOMAIN}"

if [[ $thold == true ]]; then
    config="-thowid"
elif [[ $wid -gt 0 ]]; then
    config="-dwid-${wid}"
elif [[ $nar == true ]]; then
    config="-wid+nar"
else
    config="-standard"
fi

csv_name="${csv_name}${DOMAIN}${config}"

echo "Gnerate ${csv_name} table results..."
python3 table.py -csv ${csv_name}
