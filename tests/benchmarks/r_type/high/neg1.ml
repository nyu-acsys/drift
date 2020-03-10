
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
Liquid Haskell: TODO: need to TEST!
*)

let main n =
    let g gx gy = 2 * gx in

    let twice f tx ty = f (f tx) ty in

    let neg nx ny = 0 - (nx ()) in

    let ans = if n > 0 then
        twice neg (g n) () > 0
        else false
    in
    assert (ans = true)
in main 34

(*
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)