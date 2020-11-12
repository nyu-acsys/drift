#!/bin/bash

echo "Please build the other tool before ruuning"

echo "Please also check z3 version..."
echo "For MoCHi & r_type, we used 4.7.1"

echo "Start benchmark running..."

TOOLS=" dsolve mochi r_type " #
SOLVER=" z3 hoice "
TESTDIR="../tests/benchmarks"

echo "<<----Start experiment 1---->>..."
DOMAIN=" Oct Polka_st Polka_ls "
SEN=""
for i in $(seq 2 $END); do
	if [ $i = 1 ]; then
	    echo "Result for context insensitive..."
	else
	    echo "Result for one context sensitive..."
	    SEN="-sen"
	fi
	for domain in ${DOMAIN}; do
		echo "./runit.sh -set unv -domain ${domain} ${SEN}"
		./runit.sh -set unv -domain ${domain} ${SEN}
		echo "./runit.sh -set unv -domain ${domain} -thold ${SEN}"
		./runit.sh -set unv -domain ${domain} -thold ${SEN}
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
    echo "Start running for ${tool}"
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

echo "Please check results (table1.out and table2.out) on the result folder:"
pwd