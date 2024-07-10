# Installation Requirements

- [OCaml](https://ocaml.org/), version >= 4.06
- [OPAM](https://opam.ocaml.org/), version >= 2.0.4
- [PPL](https://www.bugseng.com/ppl)
- [Apron](http://apron.cri.ensmp.fr/library/)
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/)

Please use `opam install` to install the following opam libraries:
- `menhir`, version >= 20230608
- `conf-ppl`, version >= 1
- `apron`, version >= 0.9.14
- `dune`, version >= 3.15.3

Please execute the following commands in a terminal to satisfy these requirements. We here assume an Ubuntu/Debian system but the setup for other Linux distributions and Mac OS will be very similar. The first line can be omitted if you are working from an existing opam OCaml switch >= 4.06:
```bash
opam switch create 4.11.1
opam install menhir
opam install dune
sudo apt-get install libppl-dev
opam install conf-ppl
sudo apt-get install libmpfr-dev
opam install apron
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OPAM_SWITCH_PREFIX/share/apron/lib
```

## Compilation Instructions
To produce the native code executable file, run:
```bash
dune build
```

## Usage
To test the analysis, run for example:
```bash
./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -out 2
```
or
```
./drift.exe -file tests/effects/depend.ml -prop tests/effects/depend.yml.prp -domain Polka_ls -thold true -out 2
```

### Options
```bash
usage: ./drift.exe [-file <file name>] [-domain <domain name>] [-thold <true/false>] [-delay-wid <num>] [-nar <true/false>] [-prop <file name>] [-ev-trans <true/false>] [-cps-convert <true/false>] [-out <num>] [-debug] [-bt]
  -file : Input file specification
  -domain : Abstract domain specification (Oct, Polka_st, Polka_ls, PolkaGrid)
  -thold : Use threshold widening
  -delay-wid : Set number of delay widening steps (depricated)
  -nar : Use narrowing procedure (depricated)
  -out : Output result level

         0: Output map after each step

         1: Output map only for the last step

         2: Output the result only
  -debug : Debug mode
  -profile : Profiling output
  -bt : Allow trace back
  -prop : Automata specification of safety property for effect analysis
  -ev-trans Translate Ev expressions
  -trace-len : Set maximum allowed trace length. 0 -> not context sensitive
  -if-part : Partition traces on if tokens
  -cps-convert : Convert prog to CPS
  -help  Display this list of options
  --help  Display this list of options
```

## Guides for Running Experiments 
The scripts to generate Drift-(with tuple translation) and evDrift results for tables 1 and table 2 are as follows:
1. 
2. 
```