
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
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
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)