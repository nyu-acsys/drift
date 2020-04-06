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

let main (mn:int) = 
    assert (3 * mn - 3 <= sum mn)

 let _ = main 9253
 let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)

(* Reason: wid overapproximate *)