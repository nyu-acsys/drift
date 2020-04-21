# Data Flow Refinement Type Inference Tool (DRIFT^2)
  Data Flow Refinement Type Inference Tool (DRIFT^2) is an implementation of [a novel algorithm (Page 72)](https://cs.nyu.edu/media/publications/pavlinovic_zvonimir.pdf) based on the framework of abstract interpretation for inferring refinement types in functional programs. It is an implementation of this algorithm and evaluate its precision and efficiency by comparing it to existing type inference algorithms based on Liquid types. The implementation is targeted a subset of the OCaml language and parameterized by the abstract domain used to express type refinements.

## Installation Requirements
- [OCaml](https://ocaml.org/), version >= 4.06
- [OPAM](https://opam.ocaml.org/), version >= 2.0.4
- [PPL](https://www.bugseng.com/ppl)
- [Apron](http://apron.cri.ensmp.fr/library/)
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/)

Please use the `opam install` the following opam libraries:
- `menhir`, version >= 20181113
- `conf-ppl`, version >= 1
- `apron`, version >= 0.9.10

## Instructions
To produce compiled executable file, type the following in terminal:
```bash
./build.sh
```

## Usage
To run analysis, please do the following commands inside your terminal:
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

## 1-sensitive version
Please check branch [1-sensitive](https://github.com/nyu-acsys/DART_IT/tree/1-sensitive) for more details.
