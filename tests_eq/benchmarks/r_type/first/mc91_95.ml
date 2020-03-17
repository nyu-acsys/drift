
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ m :: x:Int -> {b:Int | (b == 91)} @-}
*)
let main n =
    let rec m x =
      if x > 100
      then x - 10
      else m (m (x + 11))
    in

    if n <= 95
    then m n = 91
    else false
in assert (main 83 = true)