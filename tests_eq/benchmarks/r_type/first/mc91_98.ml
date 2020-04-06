
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ m :: x:Int -> {b:Int | (b == 91)} @-}
*)

let rec m x =
  if x > 100
  then x - 10
  else m (m (x + 11))


let main (n:int) =
    if n <= 98
    then assert (m n = 91)
    else assert(true)
let _ = main 76
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)