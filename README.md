# Data Flow Refinement Type Inference Tool (DFRTI)
  Data Flow Refinement Type Inference Tool (DFRTI) is an implementation of [a novel algorithm (Page 72)](https://cs.nyu.edu/media/publications/pavlinovic_zvonimir.pdf) based on the framework of abstract interpretation for inferring refinement types in functional programs. It is an implementation of this algorithm and evaluate its precision and efficiency by comparing it to existing type inference algorithms based on Liquid types. The implementation is targeted a subset of the OCaml language and parameterized by the abstract domain used to express type refinements.

## Installation Requirements
- [OCaml](https://ocaml.org/), version >= 4.06
- [OPAM](https://opam.ocaml.org/), version >= 2.0.4
- [Apron](http://apron.cri.ensmp.fr/library/), version >= 0.9.10
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/), version >= 20181113

## Instructions
To produce compiled executable file, type the following in terminal:
```bash
./build.sh
```

## Usage
To run analysis, please do the following commands inside your terminal:
```bash
./tests.native
```

### Options
```bash
usage: ./tests.native [-domain domain_name] [-debug] [-bt]
  -debug:  Debug mode
  -bt:     Allow trace back
  -domain: Domain specification
  -file:   Input file specification
  -help:   Display this list of options
  --help:  Display this list of options
```
