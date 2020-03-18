
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ m :: x:Int -> {b:Int | (b == 91)} @-}
*)
let main (n(*-:{v:Int | v <= 99}*)) =
    let rec m x =
      if x > 100
      then x - 10
      else m (m (x + 11))
    in

    assert (m n = 91)
(* in assert (main 1 = true) *)