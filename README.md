# Data Flow Refinement Type Inference Tool (DRIFT)
  DRIFT implements an abstract-interpretation-based static analysis
  for inferring refinement types in functional programs. The analysis
  generalizes Liquid type inference and is parametric with the
  abstract domain used to express type refinements.
  
  DRIFT targets a subset of OCaml. It currently supports
  higher-order recursive functions, operations over primitive types
  such as integers and Booleans, arrays, and lists. The tool automatically
  checks whether all array accesses are within bounds. In addition,
  it supports the verification of user-provided assertions.
  
## Installation Requirements
- [OCaml](https://ocaml.org/), version >= 4.06
- [OPAM](https://opam.ocaml.org/), version >= 2.0.4
- [PPL](https://www.bugseng.com/ppl)
- [Apron](http://apron.cri.ensmp.fr/library/)
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/)

Please use `opam install` to install the following opam libraries:
- `menhir`, version >= 20181113
- `conf-ppl`, version >= 1
- `apron`, version >= 0.9.10

Please execute the following commands in a terminal to satisfy these requirements. We here assume an Ubuntu/Debian system but the setup for other Linux distributions and Mac OS will be very similar. The first line can be omitted if you are working from an existing opam OCaml switch >= 4.06:
```bash
opam switch create 4.11.1
opam install menhir
sudo apt-get install libppl-dev
opam install conf-ppl
sudo apt-get install libmpfr-dev
opam install apron
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OPAM_SWITCH_PREFIX/share/apron/lib
```

## Compilation Instructions
To produce the native code executable file, run:
```bash
./build.sh
```

## Usage
To test the analysis, run for example:
```bash
./drift.native -file tests/benchmarks/DRIFT/high/map_2.ml -domain Polka_st -out 2
```
or
```
/drift.native -file tests/benchmarks/DRIFT/high/map_2.ml -domain Oct -sen -thold
```

### Options
```bash
usage: ./drift.native [-file <file name>] [-domain <domain name>] [-thold] [-sen] [-debug] [-bt] [-int]
  -file : Input file specification
  -domain : Abstract domain specification (Oct, Polka_st, Polka_ls)
  -sen : Use 1-context sensitive analysis
  -thold : Use threshold widening
  -out : Output result level
         0: Output map after each step
         1: Output map only for the last step
         2: Output the result only
  -debug : Debug mode
  -bt : Allow trace back
  -help       : Display this list of options
  --help      : Display this list of options
```

## Guides for Running Experiments 
The experiment to run DRIFT is working as follows:
1. We provide a shell script to run all experiments. The results we stored is inside a `output` folder.
2. We collect our data by using a python script. 
### runit.sh script for benchmark testing: 
Usage of the shell script `runit.sh` inside the **scripts** folder is:
```bash
./runit.sh -set unv -domain <domain name> [-thold] [-sen]
-set:     choose benchmark sets. The experiment used 'unv' set.
-domain:  Abstract domain specification (Oct, Polka_st, Polka_ls)
-thold:   Use threshold widening
-sen:     Use 1-context sensitive analysis
```

When it done, it will automaticallt generate a csv table with results. Please put them into corresponded subfolder inside `result` directory.
### table.py script for generating latex table:
Usage of the python script `table.py` inside the **result** folder:
```bash
# For experiment one, please run the following command:
python3 table.py unv > table.out
# For experiment two, please run the following:
python3 table.py comp_tools/unv > table.out
```
The result will be stored in a `table.out` file.

### Generate Tables for POPL paper:
Inside the `scipts` folder, there is a script `benchs_popl21.sh` to run all experiments.
```bash
./benchs_popl21.sh
```
TODO: For dsolve, you cannot run it under any other relative paths. The tool should be run it under their own directory.
