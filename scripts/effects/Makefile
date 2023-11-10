
all:
	perl makeYamls.pl
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
