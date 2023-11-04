#!/bin/bash

echo "Please note that you will need to build the other tools before ruuning this script."

echo "Please also check the z3 version... (The results reported in the paper used v. 4.7.1)"

echo "Starting experiments..."

TOOLS=" dsolve mochi r_type " #
SOLVER=" z3 hoice "
TESTDIR="../tests/benchmarks"

echo "<<----Start experiment 1---->>..."
DOMAIN=" Oct Polka_st Polka_ls "
SEN=""
for i in $(seq 2 $END); do
    if [ $i = 1 ]; then
        echo "Result for context insensitive analyses..."
    else
        echo "Result for 1-context sensitive analyses..."
        SEN="-sen"
    fi
    for domain in ${DOMAIN}; do
        echo "./runit.sh -set unv -domain ${domain} ${SEN}"
        ./runit.sh -set unv -domain ${domain} ${SEN}
        echo "./runit.sh -set unv -domain ${domain} -thold ${SEN}" 1
        ./runit.sh -set unv -domain ${domain} -thold ${SEN} 1
    done
done

echo "<<----Start experiment 2---->>..."
echo "cp ../result/1-sensitive/unv/res1-polka_ls-thowid.csv ../result/comp_tools/unv/"
cp ../result/1-sensitive/unv/res1-polka_ls-thowid.csv ../result/comp_tools/unv/

for tool in ${TOOLS}; do
    echo "Clean benchmarks..."
    pushd ${TESTDIR}
    find . -type f -not -name '*.ml' -delete
    popd
    echo "Running ${tool}"
    if [[ $tool = "mochi" || $tool = "r_type" ]]; then
        for solver in ${SOLVER}; do
            echo "./${tool}_runit.sh -set unv -${solver}"
            ./${tool}_runit.sh -set unv -${solver}
        done
    else 
        echo "./${tool}_runit.sh -set unv"
        ./${tool}_runit.sh -set unv
    fi 
done

echo "<<----Generating results---->>..."
cd ../result
echo "python3 table.py unv > table1.out"
python3 table.py unv > table1.out
echo "python3 table.py comp_tools/unv > table2.out"
python3 table.py comp_tools/unv > table2.out

echo "Please check the files table1.out and table2.out in the result directory:"
pwd