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
./drift2.native -file tests/benchmarks/DRIFT2/high/map_2.ml -domain Polka_st -out 2
```
or
```
/drift2.native -file tests/benchmarks/DRIFT2/high/map_2.ml -domain Oct -sen -thold
```

### Options
```bash
usage: ./drift2.native [-file <file name>] [-domain <domain name>] [-thold] [-sen] [-debug] [-bt] [-int]
  -file : Input file specification
  -domain : Abstract domain specification (Oct, Polka_st, Polka_ls)
  -sen : use 1-context sensitivity
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

