
all:
	rm -Rf benchmark-drift.2023*
	rm -Rf results/benchmark-*
	benchexec benchmark-drift.xml --full-access-dir /
	table-generator results/benchmark-drift.*.results.drift-new.xml.bz2 
	cp results/benchmark-drift.*.results.drift-new.html ./latest-results.html
#	unzip results/benchmark-*.zip

remakemochi:
	rm -Rf benchmark-coarmochi.2023*
	mv -f results/benchmark-* /tmp
	sudo benchexec benchmark-coarmochi.xml --full-access-dir /
	unzip results/benchmark-*.zip
