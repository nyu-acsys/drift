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

let main (n:int(*-:{v:Int | true}*)) =
    assert (3 * n - 3 <= sum n)