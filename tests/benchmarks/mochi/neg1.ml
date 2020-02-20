
(*
Res: OCaml -> 6
Our output: -6 <= v <= 6 (imprecision)
Liquid Haskell: TODO: need to TEST!
*)

let g gx gy = 2 * gx in

let twice f tx ty = f (f tx) ty in

let neg nx ny = 0 - (nx ()) in

let main n =
  let z = twice neg (g n) () in
  assert (z >= 0)
in main 3

(*
Reason why we failed: function twice contains a function parameter f.
when we strengthen the output by the input parameters, the result could not be precised.
Becasue the type of f is a table not relation, so the output will not be specified.
TODO: Will check the result for new implementation.
*)