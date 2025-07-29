import subprocess
import resource
import os
import signal
import re
import sys

def make_rethfl_config(name):
    return f"rethfl --solver=eldarica tests/effects/tr_tuple_hflz/{name}.ml"

def make_mochi_config(name):
    return f"./mochi.exe -only-result tests/effects/tr_tuple_mochi/{name}.ml"

def make_rcaml_config(name):
    return f"./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml    tests/effects/tr_tuple_rcaml/{name}.ml"

smoke_configs = [
    "./drift.exe -file tests/effects/tr_tuple_drift/all-ev-pos.ml -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/all-ev-pos.ml -prop tests/effects/all-ev-pos.yml.prp -ev-trans false -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    make_rethfl_config("all-ev-pos"),
    make_mochi_config("all-ev-pos"),
    make_rcaml_config("all-ev-pos"),

    "./drift.exe -file tests/effects/tr_tuple_drift/depend.ml -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -ev-trans false -trace-len 0 -if-part false -io-effects true -out 2 -domain Polka_ls -thold true",
    make_rethfl_config("depend"),
    make_mochi_config("depend"),
    make_rcaml_config("depend"),
    
    "./drift.exe -file tests/effects/tr_tuple_drift/overview1.ml -trace-len 1 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    "./drift.exe -file tests/effects/overview1.ml -prop tests/effects/overview1.yml.prp -ev-trans false -trace-len 0 -if-part false -io-effects false -out 2 -domain Polka_ls -thold true",
    make_rethfl_config("overview1"),
    make_mochi_config("overview1"),
    make_rcaml_config("overview1"),
]

run_mode = sys.argv[1]

if len(sys.argv) > 1 and sys.argv[1] == "smoke":
    configs = [(line.split(".ml")[0].split("/")[-1], line) for line in smoke_configs]
elif len(sys.argv) > 1 and sys.argv[1] == "table1":
    drift_configs = [line.split("|")[1] for line in open("best_configs_drift.csv").readlines() if line.strip()]
    evdrift_configs = [line.split("|")[1] for line in open("best_configs_evdrift.csv").readlines() if line.strip()]
    names = [line.split(".ml")[0].split("/")[-1] for line in drift_configs]
    rethfl_configs = [make_rethfl_config(name) for name in names]
    mochi_configs = [make_mochi_config(name) for name in names]
    rcaml_configs = [make_rcaml_config(name) for name in names]
    configs = []
    for items in zip(names, drift_configs, evdrift_configs, rethfl_configs, mochi_configs, rcaml_configs):
        name = items[0]
        items = [(name, item) for item in items[1:]]
        configs.extend(items)
elif len(sys.argv) > 1 and sys.argv[1] == "table2":
    drift_off_configs = [line.split("|")[1] for line in open("tpoff_best_configs_drift.csv").readlines() if line.strip()]
    drift_on_configs = [line.split("|")[1] for line in open("tpon_best_configs_drift.csv").readlines() if line.strip()]
    evdrift_off_configs = [line.split("|")[1] for line in open("tpoff_best_configs_evdrift.csv").readlines() if line.strip()]
    evdrift_on_configs = [line.split("|")[1] for line in open("tpon_best_configs_evdrift.csv").readlines() if line.strip()]
    names = [line.split(".ml")[0].split("/")[-1] for line in drift_off_configs]
    configs = []
    for items in zip(names, drift_off_configs, drift_on_configs, evdrift_off_configs, evdrift_on_configs):
        name = items[0]
        items = [(name, item) for item in items[1:]]
        configs.extend(items)
else:
    raise ValueError("Invalid experiment type (smoke/table1/table2)")

results_folder = "result/"
TIME_LIMIT = 900
MEMORY_LIMIT = 2 * 1024 * 1024 * 1024

def set_limits():
    # Use RLIMIT_DATA for macOS, RLIMIT_AS for Linux
    if sys.platform == "darwin":
        try:
            resource.setrlimit(resource.RLIMIT_DATA, (MEMORY_LIMIT, MEMORY_LIMIT))
        except Exception as e:
            pass  # Ignore errors on macOS
    else:
        try:
            resource.setrlimit(resource.RLIMIT_AS, (MEMORY_LIMIT, MEMORY_LIMIT))
        except Exception as e:
            pass  # Ignore errors on Linux

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

for (name, config) in configs:
    if run_mode == "table2":
        if "tr_tuple" in config:
            if "if-part false" in config:
                mode = "drift_false"
            else:
                mode = "drift_true"
        else:
            if "if-part false" in config:
                mode = "evdrift_false"
            else:
                mode = "evdrift_true"
    else:
        if "drift" in config:
            if "tr_tuple" in config:
                mode = "drift"
            else:
                mode = "evDrift"
        elif "rethfl" in config:
            mode = "rethfl"
        elif "mochi" in config:
            mode = "mochi"
        else:
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

for (name, config) in configs:
    if run_mode == "table2":
        if "tr_tuple" in config:
            if "if-part false" in config:
                mode = "drift_false"
            else:
                mode = "drift_true"
        else:
            if "if-part false" in config:
                mode = "evdrift_false"
            else:
                mode = "evdrift_true"
    else:
        if "drift" in config:
            if "tr_tuple" in config:
                mode = "drift"
            else:
                mode = "evDrift"
        elif "rethfl" in config:
            mode = "rethfl"
        elif "mochi" in config:
            mode = "mochi"
        else:
            mode = "rcaml"

    output_file = results_folder + name + "_" + mode + ".txt"
    if name not in table:
        table[name] = {"drift": "", "evDrift": "", "rethfl": "", "mochi": "", "rcaml": ""}
    try:
        with open(output_file, 'r') as f:
            content = f.read().strip()
            if run_mode == "table2":
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
            else:
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
                elif mode == "rethfl":
                    if "TimeoutExpired" in content:
                        result = "timeout"
                    elif "Invalid" in content:
                        result = "unknown"
                    elif "Valid" in content:
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

if run_mode == "table2":
    header = ["name", "drift-off", "drift-on", "evdrift-off", "evdrift-on"]
else:
    header = ["name", "drift", "rcaml", "mochi", "rethfl", "evDrift"]

rows = []
for name in sorted(table.keys()):
    if run_mode == "table2":
        row = [name, table[name]["drift-off"], table[name]["drift-on"], table[name]["evdrift-off"], table[name]["evdrift-on"]]
    else:
        row = [name, table[name]["drift"], table[name]["rcaml"], table[name]["mochi"], table[name]["rethfl"], table[name]["evDrift"]]
    rows.append(row)

col_widths = [max(len(str(row[i])) for row in ([header] + rows)) for i in range(len(header))]

print("  ".join(header[i].ljust(col_widths[i]) for i in range(len(header))))
print("-" * (sum(col_widths) + 2 * (len(header) - 1)))

for row in rows:
    print("  ".join(str(row[i]).ljust(col_widths[i]) for i in range(len(row))))

