
remake:
	rm -Rf benchmark-drift.2023*
	mv -f results/benchmark-* /tmp
	sudo benchexec benchmark-drift.xml --full-access-dir /
	unzip results/benchmark-*.zip

remakemochi:
	rm -Rf benchmark-coarmochi.2023*
	mv -f results/benchmark-* /tmp
	sudo benchexec benchmark-coarmochi.xml --full-access-dir /
	unzip results/benchmark-*.zip
