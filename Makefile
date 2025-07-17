all: 
	dune build

clean: 
	dune clean

#
# Simple experiments that can be run in Docker
#
simple_smoke:
	./run_experiments.py smoke

simple_table1:
	./run_experiments.py table1

simple_table2:
	./run_experiments.py table2

#
# Full expensive experiments that require BenchExec
#
best_config_csvs:
	cd scripts/effects && make best_config_csvs
	cp scripts/effects/best_configs_drift.csv .
	cp scripts/effects/best_configs_evdrift.csv .
	cp scripts/effects/exp-apx.tex .

best_trace_part_configs:
	cd scripts/effects && make best_trace_part_configs
	cp scripts/effects/best_configs_tp_drift.csv .
	cp scripts/effects/best_configs_tp_evdrift.csv .
	cp scripts/effects/best_configs_notp_drift.csv .
	cp scripts/effects/best_configs_notp_evdrift.csv .

precise:
	cd scripts/effects && make precise

precise_results:
	cd scripts/effects && make precise_results
	cp scripts/effects/exp-body.tex .
	cp scripts/effects/exp-stats.tex .

trace_part:
	cd scripts/effects && make trace_part
	cp scripts/effects/exp-tp.tex .
	cp scripts/effects/exp-tpstats.tex .