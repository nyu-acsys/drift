(*
Res: OCaml -> 284635
imprecision: Oct: 754 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)

let main (mn:int(*-:{v:Int | true}*)) =
    assert (4 * mn - 6 <= sum mn)

let _ = main 754

(* Reason: wid overapproximate *)