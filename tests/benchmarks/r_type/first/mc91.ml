(*
USED: PLDI2011 as mc91
USED: PEPM2013 as mc91

Res: OCaml -> 91
imprecision: Oct with delay widening: 91 <= v <= 101
Each test domain without widening: 91 <= v
Liquid Haskell: false by property {-@ mc91 :: x:Int -> {b:Int | (b == 91)} @-}
*)
let main (n(*-:{v:Int | v <= 101 }*)) =
    let rec mc91 x =
      if x > 100
      then x - 10
      else mc91 (mc91 (x + 11))
    in

    assert (mc91 n = 91)

(* in assert (main 1 = true) *)