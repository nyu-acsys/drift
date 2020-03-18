
(*
Res: OCaml -> 6
imprecision: Oct: -6 <= v <= 6
Liquid Haskell: TODO: need to TEST!
*)

let main (n(*-:{v:Int | v > 0}*)) =
    let g gx gy = 2 * gx in

    let twice f tx ty = f (f tx) ty in

    let neg nx ny = 0 - (nx ()) in

    assert (twice neg (g n) () > 0)
(* in main 34 *)

(*
Reason: strenthen in here is table not relation, so the call-site could not be specified for f
*)