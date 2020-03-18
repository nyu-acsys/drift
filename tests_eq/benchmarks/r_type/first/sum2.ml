
(*
Res: OCaml -> 10453878
imprecision: Oct: 4752 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)

let main mn = 
    assert (2 * mn - 1 <= sum mn)

let _ = main 4752

(* Reason: wid overapproximate *)