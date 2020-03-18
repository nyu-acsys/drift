(*
Res: OCaml -> 42813631
imprecision: Oct: 9253 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)

let main mn = 
    assert (3 * mn - 3 <= sum mn)

let _ = main 9253

(* Reason: wid overapproximate *)