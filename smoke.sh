# Drift on all-ev-pos:
./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true
# evDrift on all-ev-pos:
./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true
# Drift on depend:
./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true
# evDrift on depend:
./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true
# Drift on overview1:
./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true
# evDrift on overview1:
./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true
