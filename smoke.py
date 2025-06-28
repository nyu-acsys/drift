import subprocess
import resource
import os
import signal

configs = [
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

results_folder = "results/"
time_limit = 900
memory_limit = 2 * 1024 * 1024 * 1024

def set_limits():
    resource.setrlimit(resource.RLIMIT_CPU, (TIME_LIMIT, TIME_LIMIT))
    resource.setrlimit(resource.RLIMIT_AS, (MEMORY_LIMIT, MEMORY_LIMIT))

def run_command_with_limits(cmd):
    try:
        result = subprocess.run(
            cmd,
            shell=True,
            preexec_fn=set_limits,  # Only works on Unix/Linux
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=TIME_LIMIT + 2,  # A bit more than CPU limit
            text=True
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "TimeoutExpired"
    except MemoryError:
        return -1, "", "MemoryError"
    except Exception as e:
        return -1, "", str(e)

for config in configs:
    name = config.split("tests/effects/")[1].split(".ml")[0]
    if "drift" in config:
        if "trans" in config:
            mode = "drift"
        else:
            mode = "evDrift"
    elif "mochi" in config:
        mode = "mochi"
    else:
        mode = "rcaml"

    output_file = results_folder + name + "_" + mode + ".txt"
    cmd = config + " > " + output_file

    code, out, err = run_command_with_limits(cmd)
    if code = -1:
        with open(output_file, "w") as f:
            f.write(err*"\n")
    
