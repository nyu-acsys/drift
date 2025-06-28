import subprocess
import resource
import os
import signal
import re
import sys

smoke_configs = [
    "./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/all-ev-pos.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/all-ev-pos.ml",
    "./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/depend.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/depend.ml",
    "./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/overview1.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/overview1.ml"
]

table1_configs = [

    "./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/all-ev-pos.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/all-ev-pos.ml",

    "./drift.exe -file tests/effects/alt-inev.ml -prop tests/effects/alt-inev.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/alt-inev.ml -prop tests/effects/alt-inev.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/alt-inev.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/alt-inev.ml",

    "./drift.exe -file tests/effects/auction.ml -prop tests/effects/auction.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/auction.ml -prop tests/effects/auction.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/auction.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/auction.ml",

    "./drift.exe -file tests/effects/binomial_heap.ml -prop tests/effects/binomial_heap.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/binomial_heap.ml -prop tests/effects/binomial_heap.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/binomial_heap.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/binomial_heap.ml",

    "./drift.exe -file tests/effects/concurrent_sum.ml -prop tests/effects/concurrent_sum.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/concurrent_sum.ml -prop tests/effects/concurrent_sum.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/concurrent_sum.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/concurrent_sum.ml",

    "./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/depend.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/depend.ml",

    "./drift.exe -file tests/effects/disj.ml -prop tests/effects/disj.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/disj.ml -prop tests/effects/disj.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain PolkaGrid -thold false",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/disj.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/disj.ml",

    "./drift.exe -file tests/effects/disj-gte.ml -prop tests/effects/disj-gte.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/disj-gte.ml -prop tests/effects/disj-gte.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/disj-gte.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/disj-gte.ml",

    "./drift.exe -file tests/effects/disj-nondet.ml -prop tests/effects/disj-nondet.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/disj-nondet.ml -prop tests/effects/disj-nondet.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/disj-nondet.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/disj-nondet.ml",

    "./drift.exe -file tests/effects/higher-order.ml -prop tests/effects/higher-order.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/higher-order.ml -prop tests/effects/higher-order.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/higher-order.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/higher-order.ml",

    "./drift.exe -file tests/effects/last-ev-even.ml -prop tests/effects/last-ev-even.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/last-ev-even.ml -prop tests/effects/last-ev-even.yml.prp -ev-trans direct -trace-len 1 -if-part true -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/last-ev-even.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/last-ev-even.ml",

    "./drift.exe -file tests/effects/lics18-amortized.ml -prop tests/effects/lics18-amortized.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/lics18-amortized.ml -prop tests/effects/lics18-amortized.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/lics18-amortized.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/lics18-amortized.ml",

    "./drift.exe -file tests/effects/lics18-hoshrink.ml -prop tests/effects/lics18-hoshrink.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/lics18-hoshrink.ml -prop tests/effects/lics18-hoshrink.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/lics18-hoshrink.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/lics18-hoshrink.ml",

    "./drift.exe -file tests/effects/lics18-web.ml -prop tests/effects/lics18-web.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/lics18-web.ml -prop tests/effects/lics18-web.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/lics18-web.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/lics18-web.ml",

    "./drift.exe -file tests/effects/market.ml -prop tests/effects/market.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/market.ml -prop tests/effects/market.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/market.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/market.ml",

    "./drift.exe -file tests/effects/max-min.ml -prop tests/effects/max-min.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/max-min.ml -prop tests/effects/max-min.yml.prp -ev-trans direct -trace-len 1 -if-part true -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/max-min.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/max-min.ml",

    "./drift.exe -file tests/effects/monotonic.ml -prop tests/effects/monotonic.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/monotonic.ml -prop tests/effects/monotonic.yml.prp -ev-trans direct -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/monotonic.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/monotonic.ml",

    "./drift.exe -file tests/effects/nondet_max.ml -prop tests/effects/nondet_max.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/nondet_max.ml -prop tests/effects/nondet_max.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/nondet_max.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/nondet_max.ml",

    "./drift.exe -file tests/effects/order-irrel.ml -prop tests/effects/order-irrel.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/order-irrel.ml -prop tests/effects/order-irrel.yml.prp -ev-trans direct -trace-len 1 -if-part true -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/order-irrel.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/order-irrel.ml",

    "./drift.exe -file tests/effects/order-irrel-nondet.ml -prop tests/effects/order-irrel-nondet.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/order-irrel-nondet.ml -prop tests/effects/order-irrel-nondet.yml.prp -ev-trans direct -trace-len 1 -if-part true -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/order-irrel-nondet.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/order-irrel-nondet.ml",

    "./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans trans -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/overview1.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/overview1.ml",

    "./drift.exe -file tests/effects/reentr.ml -prop tests/effects/reentr.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/reentr.ml -prop tests/effects/reentr.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/reentr.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/reentr.ml",

    "./drift.exe -file tests/effects/resource-analysis.ml -prop tests/effects/resource-analysis.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/resource-analysis.ml -prop tests/effects/resource-analysis.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/resource-analysis.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/resource-analysis.ml",

    "./drift.exe -file tests/effects/sum-appendix.ml -prop tests/effects/sum-appendix.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/sum-appendix.ml -prop tests/effects/sum-appendix.yml.prp -ev-trans direct -trace-len 1 -if-part true -io-effects true -out 2 -domain Polka_ls -thold true",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/sum-appendix.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/sum-appendix.ml",

    "./drift.exe -file tests/effects/sum-of-ev-even.ml -prop tests/effects/sum-of-ev-even.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/sum-of-ev-even.ml -prop tests/effects/sum-of-ev-even.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/sum-of-ev-even.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/sum-of-ev-even.ml",

    "./drift.exe -file tests/effects/temperature.ml -prop tests/effects/temperature.yml.prp -ev-trans trans -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./drift.exe -file tests/effects/temperature.ml -prop tests/effects/temperature.yml.prp -ev-trans direct -trace-len 0 -if-part false -io-effects false -out 2 -domain PolkaGrid -thold false",
    "./mochi.exe -only-result tests/effects/tr_tuple_mochi/temperature.ml",
    "./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/temperature.ml"
]

if len(sys.argv) > 1 and sys.argv[1] == "smoke":
    configs = smoke_configs
elif len(sys.argv) > 1 and sys.argv[1] == "all":
    configs = table1_configs
else:
    configs = table1_configs

results_folder = "result/"
TIME_LIMIT = 900
MEMORY_LIMIT = 2 * 1024 * 1024 * 1024

def set_limits():
    resource.setrlimit(resource.RLIMIT_AS, (MEMORY_LIMIT, MEMORY_LIMIT))

def run_command_with_limits(cmd):
    try:
        result = subprocess.run(
            cmd,
            shell=True,
            preexec_fn=set_limits,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=TIME_LIMIT + 2,
            text=True
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "TimeoutExpired"
    except Exception as e:
        return -1, "", str(e)

for config in configs:
    if "drift" in config:
        name = config.split("tests/effects/")[1].split(".ml")[0]
        if "-ev-trans trans" in config:
            mode = "drift"
        else:
            mode = "evDrift"
    elif "mochi" in config:
        config.split("tests/effects/tr_tuple_mochi/")[1].split(".ml")[0]
        mode = "mochi"
    else:
        config.split("tests/effects/tr_tuple_rcaml/")[1].split(".ml")[0]
        mode = "rcaml"

    output_file = results_folder + name + "_" + mode + ".txt"
    cmd = config

    print("Running "+mode+" on "+name)
    code, out, err = run_command_with_limits(cmd)
    if code == -1:
        with open(output_file, "w") as f:
            # print(err)
            f.write(err+"\n")
    else:
        with open(output_file, "w") as f:
            # print(out)
            f.write(out+"\n")

table = {}

pgood_rcaml = re.compile('sat,-?\d+\s+\@assert')
pexcep_rcaml = re.compile('Uncaught exception')

for config in configs:
    if "drift" in config:
        name = config.split("tests/effects/")[1].split(".ml")[0]
        if "-ev-trans trans" in config:
            mode = "drift"
        else:
            mode = "evDrift"
    elif "mochi" in config:
        name = config.split("tests/effects/tr_tuple_mochi/")[1].split(".ml")[0]
        mode = "mochi"
    else:
        name = config.split("tests/effects/tr_tuple_rcaml/")[1].split(".ml")[0]
        mode = "rcaml"

    output_file = results_folder + name + "_" + mode + ".txt"
    if name not in table:
        table[name] = {"drift": "", "evDrift": "", "mochi": "", "rcaml": ""}
    try:
        with open(output_file, 'r') as f:
            content = f.read().strip()
            if mode in ["drift", "evDrift"]:
                if "TimeoutExpired" in content:
                    result = "timeout"
                elif "The input program is safe" in content:
                    result = "verif."
                elif "The program may not be safe" in content:
                    result = "unknown"
                elif "MemoryError" in content:
                    result = "memout/error"
                else:
                    result = "memout/error"
            elif mode == "rcaml":
                if "TimeoutExpired" in content:
                    result = "timeout"
                elif pgood_rcaml.match(content):
                    result = "verif."
                elif pexcep_rcaml.match(content):
                    result = "Excep"
                elif "MemoryError" in content:
                    result = "memout/error"
                else:
                    result = "memout/error"
            elif mode == "mochi":
                if "TimeoutExpired" in content:
                    result = "timeout"
                elif "Safe!" in content:
                    result = "verif."
                elif "MemoryError" in content:
                    result = "memout/error"
                else:
                    result = "memout/error"
            else:
                result = "memout/error"

    except Exception:
        result = "memout/error"
    table[name][mode] = result

header = ["name", "drift", "rcaml", "mochi", "evDrift"]
rows = []
for name in sorted(table.keys()):
    row = [name, table[name]["drift"], table[name]["rcaml"], table[name]["mochi"], table[name]["evDrift"]]
    rows.append(row)

col_widths = [max(len(str(row[i])) for row in ([header] + rows)) for i in range(len(header))]

print("  ".join(header[i].ljust(col_widths[i]) for i in range(len(header))))
print("-" * (sum(col_widths) + 2 * (len(header) - 1)))

for row in rows:
    print("  ".join(str(row[i]).ljust(col_widths[i]) for i in range(len(row))))

