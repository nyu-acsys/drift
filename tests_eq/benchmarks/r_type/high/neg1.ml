
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
Liquid Haskell: TODO: need to TEST!
*)


let g gx gy = 2 * gx

let twice f tx ty = f (f tx) ty

let neg nx ny = 0 - (nx ())

let main n =
    if n > 0 then
    	assert (twice neg (g n) () > 0)

let _ = main 34

(*
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)