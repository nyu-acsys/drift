
remake:
	rm -Rf benchmark-drift.2023*
	mv results/benchmark-* /tmp
	sudo benchexec benchmark-drift.xml --read-only-dir /
	unzip results/benchmark-*.zip

remakemochi:
	rm -Rf benchmark-coarmochi.2023*
	mv results/benchmark-* /tmp
	sudo benchexec benchmark-coarmochi.xml --read-only-dir /
	unzip results/benchmark-*.zip
