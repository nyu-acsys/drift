# The ev-Drift Verifier

This tool uses a type-and-effect system and abstract interpretation to 
infer symbolic accumulation automaton (SAA) properties of higher order programs.

An example program can be seen in `tests/effects/overview.ml`

An example SAA property can be seen in `tests/effects/overview.yml.prp`

In addition, one can use our tuple-based product translation to 
transform these two inputs into an effectless program with assertions,
and use the Drift tool (POPL 2020). This can be done by suppling
`-ev-trans true` as a commandline argument (see details below).

## Installation Requirements

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
```
usage: ./drift.exe [-file <file path>] [-domain <Oct/Polka_st/Polka_ls/PolkaGrid>] [-thold <true/false>] [-prop <file name>] [-trace-len <num>] [-if-part <true/false>] [-io-effects <true/false>] [-ev-trans <true/false>] [-cps-convert <true/false>] [-tuple-convert <true/false>] [-out <num:[0,2]>] [-debug] [-bt]
  -file : Input file path
  -domain : Abstract domain specification (Oct/Polka_st/Polka_ls/PolkaGrid). "Oct" stands for the Octagon domain. "Polka_st" stands for the strict-Polyhedra domain. "Polka_ls" stands for the loose-Polyhedra domain.
  "PolkaGrid" stands for the grid-polyhedra domain - a product domain composed of the loose-Polyhedra and Grid domains.
  -thold : Use widening with thresholds (Plain widening is default). Increases precision, but also increases running time. Note: not supported for PolkaGrid domain.
  -if-part : Partition traces on if tokens. Increases precision, but also increases running time.
  -trace-len : Number of call-sites and if-partitions (if enabled) remembered. 0 -> not context sensitive. Increasing this increases precision, but also increases running time.
  -prop : Automata specification file path.
  -ev-trans : Translate Ev expressions (run Drift with tuple translation).
  -io-effects : Input-output relations for effects for greater precision. Increases precision, but also increases running time. Has no effect when ev-trans is true.
  -cps-convert : Convert prog to CPS
  -tuple-convert : Convert prog to tuple-encoding of the product program
  -out : Output result level (See below for a description of what this output means).
    0: Output map after each step
    1: Output map only for the last step
    2: Output the result only
  -debug : Debug mode
  -profile : Profiling output.
  -bt : Allow trace back
  -help  Display this list of options
  --help  Display this list of options
```
### Input

#### Program file

Our tool supports integers, booleans, and tuples, along with higher-order recursive functions and if-then-else expressions.
Our example programs provide a good subset of the language terms that are properly supported.
One can extend the language by adding analysis for the respective clauses in the `step` function in `abstractTransformer.ml` file.
Additional abstract domains might also be needed for other data types like strings, lists and arrays, which can be added in the `semanticDomain.ml` (interface with the abstract transformer) and `abstractDomain.ml` (interface with the Apron library) files.
See the Drift paper [[1]](#1) for more details.

#### Config file

TODO

### Output levels
- Level 2: Only returns the result: whether the program is "safe", i.e. all assertions are satisfied. Or, the program "might be unsafe", i.e., some assertions may or may not be satisfied.
- Level 1: Also produces the final execution map (See [[1]](#1) for more details.).
An execution map maps every program node by location (the program locations are assigned statically) to its refinement type-and-effect.
When `trace-len` is not zero, every node is tagged by a context, which the map shows in the following syntax:

  `EN: 31;z180..251_true`

  This denotes execution node 31 (we also have variable nodes denoted by `VN` that are essentially the nodes where functions are defined. See [[1]](#1) for more details.), reached through call-site at location 180, and taking the true branch at the if-block at 281. There may be multiple call-sites to the left of, and/or multiple if-blocks to the right of `..`.

  For evDrift, this may have a type-and-effect that may look as follows:
```
  (
  t: [ z39..:
         ((
          t: { cur_v: Int | [|-l2_deq+x11=0; x10=0; l1_deq=0; cur_v-l2_deq+l_aux=0; -cur_v+l2_deq>=0; -l2_deq+prefl2+prefn>=0; prefn>=0; prefl2>=0; prefl1>=0; cur_v-1>=0|] },
          eff: ( 0 |->
                [|-deq-l2_deq+l_aux+prefl1+tick=0; -deq+enq-l2_deq+prefl1+prefl2=0; -enq+prefn=0; -l2_deq+x11=0; -l2_deq+l_aux+z39..=0; x10=0; l1_deq=0; prefl1>=0; l_aux>=0; l2_deq-l_aux-1>=0;
                  enq>=0; deq-enq+l2_deq-prefl1>=0; deq-prefl1>=0|])) ->
         < ..:
             (
             t: { cur_v: Int | [|-cur_v+x11=0; -cur_v+l_aux+z39..=0; -cur_v+l2_deq=0; x10=0; l1_deq=0; -cur_v+prefl2+prefn>=0; prefn>=0; prefl2>=0; prefl1>=0; l_aux>=0; cur_v-l_aux-1>=0|] },
             eff: ( 0 |->
                   [|-deq-l2_deq+prefl1+tick=0; -deq+enq-l2_deq+prefl1+prefl2=0; -enq+prefn=0; -l2_deq+x11=0; -l2_deq+l_aux+z39..=0; x10=0; l1_deq=0; prefl1>=0; l_aux>=0; l2_deq-l_aux-1>=0; 
                     enq>=0; deq-enq+l2_deq-prefl1>=0; deq-prefl1>=0|])) >) ],
  eff: ( 0 |->
        [|-deq-l2_deq+l_aux+prefl1+tick=0; -deq+enq-l2_deq+prefl1+prefl2=0; -enq+prefn=0; -l2_deq+x11=0; x10=0; l1_deq=0; prefl1>=0; l_aux>=0; l2_deq-l_aux-1>=0; enq>=0; deq-enq+l2_deq-prefl1>=0;
          deq-prefl1>=0|]))
  ```

  `t` shows the type, and `eff` shows the effect.
  
  In this case, the node is a table type (indicated by the square brackets), which has a single call-site at location 39, with input type-and-effect as shown above (Note that it is an integer type). In cases of integer types, the node in question appears as `cur_v` in the constraint.
  The input type can appear in the abstract environment of the input effect (See z39..), and can also appears in the output type.
  The output type (separated by a `->`) is a mapping of type-and-effects by contexts (think different branches taken within a function produce different outputs). In this case, there might have been no branchings in the function, so we just have a single entry `..`.

  Effects are mappings from state to abstract environments that additionally include the accumulator variables (not `cur_v` in this case, rather the names of the accumulator variables.).

- Level 0: Show the intermediate execution maps after every iteration of the algorithm.

## References
<a id="1">[1]</a> 
Pavlinovic, Zvonimir and Su, Yusen and Wies, Thomas (2021). Data flow refinement type inference. Proceedings of the ACM on Programming Languages.