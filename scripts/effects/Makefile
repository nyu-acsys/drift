
precise:
	perl makeYamls.pl
	benchexec benchmark-driftwrapper.xml @benchexec.cfg
	benchexec benchmark-coarmochi.xml @benchexec.cfg
	benchexec benchmark-mochi.xml @benchexec.cfg
	benchexec benchmark-rethfl.xml @benchexec.cfg
	echo " "
	echo "  BenchExec complete for Drift, evDrift, RCaml, Mochi, and Rethfl."
	echo "  Now parse the results by typing:"
	echo " "
	echo "      make precise_results"
	echo " "

precise_results:
	# Run the BenchExec table generator for the most recently generated
	table-generator `ls -t results/benchmark-rethfl.202*.xml.bz2 | head -n 1`
	table-generator `ls -t results/benchmark-coarmochi.202*.xml.bz2 | head -n 1`
	table-generator `ls -t results/benchmark-mochi.202*.xml.bz2 | head -n 1`
	table-generator `ls -t results/benchmark-driftwrapper.202*.xml.bz2 | head -n 1`
	# Run makeLatex on the most recent 4 CSVs
	echo "`ls -t results/benchmark-rethfl.202*.csv | head -n 1`" > csvs.txt
	echo "`ls -t results/benchmark-coarmochi.202*.csv | head -n 1`" >> csvs.txt
	echo "`ls -t results/benchmark-mochi.202*.csv | head -n 1`" >> csvs.txt
	echo "`ls -t results/benchmark-driftwrapper.202*.csv | head -n 1`" >> csvs.txt
	perl makeLatex.pl --csvs csvs.txt

bestconfig:
	perl makeYamls.pl
	perl makeRunDefinitions.pl
	rm -Rf benchmark-drift.2023*/*
	rm -Rf results/benchmark-*
	benchexec benchmark-drift.xml --full-access-dir / --overlay-dir /home
	table-generator results/benchmark-drift.*.results.drift-new.effects.xml.bz2 
	cp results/benchmark-drift.*.results.drift-new.effects.html ./latest-results.html
	rm -Rf benchmark-drift*.logfiles
	unzip -f results/benchmark-drift*.zip

remakemochi:
	rm -Rf benchmark-coarmochi.2023*
	mv -f results/benchmark-* /tmp
	sudo benchexec benchmark-coarmochi.xml --full-access-dir /
	unzip results/benchmark-*.zip
