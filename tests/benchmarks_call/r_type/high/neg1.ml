
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
Liquid Haskell: TODO: need to TEST!
*)

let g (gx:int) (gy:unit) = 2 * gx

let twice f (tx:unit->int) (ty:unit) = f (f tx) ty

let neg nx (ny:unit) = 0 - (nx ())

let main_p (n:int) =
	let z = twice neg (g n) () in
    if n > 0 then
    	assert (z > 0)
    else assert (z <= 0)

let main (w:unit) =
	let _ = main_p 34 in
    let _ = main_p (-10) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
(*
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)