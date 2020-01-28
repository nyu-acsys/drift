
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ m :: x:Int -> {b:Int | (b == 91)} @-}
*)

let rec m x =
  if x > 100
  then x - 10
  else m (m (x + 11))
in
let main n =
  if n <= 98
  then assert (m n = 91)
  else false
in main 76