
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
Liquid Haskell: TODO: need to TEST!
*)

let g (gx:int) (gy:unit) = 2 * gx

let twice f (tx:unit->int) (ty:unit) = f (f tx) ty

let neg nx (ny:unit) = 0 - (nx ())

let main (n:int(*-:{v:Int | true}*)) =
	let z = twice neg (g n) () in
    if n > 0 then
    	assert (z > 0)
    else assert (z <= 0)

(*
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)