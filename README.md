# Data Flow Refinement Type Inference Tool (DRIFT^2)
  DRIFT^2 implements an abstract-interpretation-based static analysis
  for inferring refinement types in functional programs. The analysis
  generalizes Liquid type inference and is parametric with the
  abstract domain used to express type refinements.
  
  DRIFT^2 targets a subset of OCaml. It currently supports
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

### Options
```bash
usage: ./tests.native [-file input file name] [-domain domain_name] [-nar true/false] [-delay-wid NUM] [-debug] [-bt] [-int]
  -debug      : Debug mode
  -bt         : Allow trace back
  -domain     : Domain specification
  -delay-wid  : Set delay widening steps
  -int        : Display the final result only
  -nar        : Use narrowing procedure
  -file       : Input file specification
  -help       : Display this list of options
  --help      : Display this list of options
```

