(*
USED: PLDI2011 as mc91
USED: PEPM2013 as mc91

Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ mc91 :: x:Int -> {b:Int | (b == 91)} @-}
*)
let main n =
    let rec mc91 x =
      if x > 100
      then x - 10
      else mc91 (mc91 (x + 11))
    in

    if n <= 101
    then mc91 n = 91
    else false
in assert (main 1 = true)