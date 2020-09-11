# Data Flow Refinement Type Inference Tool (DRIFT)
  DRIFT implements an abstract-interpretation-based static analysis
  for inferring refinement types in functional programs. The analysis
  generalizes Liquid type inference and is parametric with the
  abstract domain used to express type refinements.
  
  DRIFT targets a subset of OCaml. It currently supports
  higher-order recursive functions, operations over primitive types
  such as integers and Booleans, and arrays. The tool automatically
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

## Compilation Instructions
To produce the native code executable file, run:
```bash
./build.sh
```

## Usage
To test the analysis, run for example:
```bash
./tests.native -file tests/benchmarks/DART_IT/high/map_2.ml -domain Polka_st
```
(context-insensitive, Polka strict, default widening)

```bash
./tests.native -file tests/benchmarks/DART_IT/high/map_2.ml -domain Oct -sen -thold
```
(context-sensitive, Octagons, threshold widening)




### Options
```bash
usage: ./drift2.native [-file <file name>] [-domain <domain name>] [-thold] [-sen] [-debug] [-bt] [-int]
  -file       : Input file specification
  -domain     : Abstract domain specification (Oct, Polka_st, Polka_ls)
  -thold      : Use threshold widening
  -sen        : use 1-context sensitivity
  -debug      : Debug mode
  -bt         : Allow trace back
  -int        : Display the final result only
  -help       : Display this list of options
  --help      : Display this list of options
```

