(lang dune 3.15)
(using menhir 2.1)

(map_workspace_root true)

(source (github nyu-acsys/drift))

(generate_opam_files true)

(license BSD-3-Clause)

(authors 
  "Mihai Nicola" 
  "Chaitanya Agarwal" 
  "Eric Koskinen" 
  "Thomas Wies")

(maintainers 
  "Mihai Nicola" 
  "Chaitanya Agarwal" 
  "Eric Koskinen" 
  "Thomas Wies")

(package
 (name evDrift)
 (synopsis "Abstract-interpretation-based verification of temporal safety properties of effectful higher-order programs")
 (description "evDrift is a static analysis tool for verification of temporal properties of higher order programs, build on top of the data flow refinement type inference tool Drift. It takes as input a program in a subset of OCaml extended with 'ev' expressions, along with a temporal safety property specified expressed as an Symbolic Accumulator Automaton (SAA), where an SAA is defined by a set of states, a deterministic transition function as nested if-then-else expressions and two kind of effect-related assertions.")
 (depends 
  (ocaml (>= 4.11.1))
  (menhir (>= 20230608))
  (conf-ppl (>= 1))
  (apron (= 0.9.14))
))




