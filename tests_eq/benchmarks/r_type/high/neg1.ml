
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
Liquid Haskell: TODO: need to TEST!
*)


let g (gx:int) (gy:unit) = 2 * gx

let twice f (tx:unit->int) (ty:unit) = f (f tx) ty

let neg nx (ny:unit) = 0 - (nx ())

let main (n:int) =
    if n > 0 then
    	assert (twice neg (g n) () > 0)
    else assert(true)

let _ = main 34
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)

(*
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)