
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

let main (n:int(*-:{v:Int | true}*)) =
    assert (2 * n - 1 <= sum n)