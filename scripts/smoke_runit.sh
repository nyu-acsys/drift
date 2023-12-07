#!/bin/bash

# ./runit.sh -set call/unv -domain Oct [-trace-len <trace_length>] [-nar | -dwid 0 | -thold] [-if-part]
# Domain for benchmarks: Oct, Polka_st, Polka_ls
opam switch 4.08.1
OUTDIR="../outputs/DRIFT"
PROG="../drift.native"
PROGNAME="DRIFT"
SET=$2
DOMAIN=$4
if [ $# -gt 1 ]; then
    SET=$2
    DOMAIN=$4
else
    echo "ERROR!!! The command should be:"
    echo "./runit.sh -trace-len call/unv -domain <domain_name> [-trace-len <trace_length>] [-dwid <delay_steps> | -nar] [-if-part]"
    exit 0
fi
shift
shift
shift
shift

wid=0
nar=false
thold=false
trace_len=0
if_part=false

if [[ $# -ge 2 && ($1 = "-trace-len")]]; then
    trace_len=$2;
    echo "Use $trace_len sensitive"
    shift
    shift
fi

if [ $# -ge 1 ] && [ $1 = "-nar" ]; then
    echo "Wid + Nar..."
    wid=0;
    nar=true;
    shift
elif [ $# -ge 1 ] && [ $1 = "-thold" ]; then
    echo "Wid with threshold..."
    thold=true;
    nar=false;
    wid=0;
    shift
else 
    if [ $# -ge 2 ] && [ $1 = "-dwid" ]; then
        echo "Delay widening + Nar. Widen steps occur after $2"
        nar=true;
        wid=$2;
        shift
        shift
    else 
        echo "Default. Standard widening..."
    fi
fi

if [ $# -ge 1 ] && [ $1 = "-if-part" ]; then
    echo "If Partitioning"
    if_part=true;
fi

echo "outdir=<$OUTDIR> prog=<$PROG>"

if [ $SET = "call" ]; then
    TESTDIR="../tests/benchmarks_call"
else
    TESTDIR="../tests/benchmarks"
fi

DIRS=" DRIFT DOrder r_type"
INS=" array list first high negative " # 
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
            if [ ${dir} = "array" ] && [ ${hdir} != ${PROGNAME} ]; then
                continue
            fi
            for f in `find ${TESTDIR}/${hdir}/${dir} -iname "*.ml" -type f -execdir echo '{}' ';'`; do
                if [[ "$OSTYPE" != "darwin"* ]]; then
                    f=${f#*./}
                fi
                echo "${PROG} -file ${TESTDIR}/${hdir}/${dir}/${f} -out 2 -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar} -thold ${thold} -trace-len ${trace_len}"
                ts=$(${DATE} +%s%N)
                timeout ${timeout} ${PROG} -file ${TESTDIR}/${hdir}/${dir}/${f} -out 2 -domain ${DOMAIN} -delay-wid ${wid} -nar ${nar} -thold ${thold} -trace-len ${trace_len} &> ${OUTDIR}/${dir}/${OUTPRE}_${f}
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

if [[ $trace_len > 0 ]]; then
    csv_name="${csv_name}${trace_len}-"
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

if [[ $trace_len > 0 ]]; then
    RESDIR="${trace_len}-sensitive"
else
    RESDIR="non-sensitive"
fi

if [ $SET = "call" ]; then
    RESSUBDIR="call"
else
    RESSUBDIR="unv"
fi

csv_name="${csv_name}${DOMAIN}${config}"

echo "Generate ${csv_name} table results..."
python3 table.py -csv ${csv_name}

mv ../${csv_name}.csv ../result/${RESDIR}/${RESSUBDIR}