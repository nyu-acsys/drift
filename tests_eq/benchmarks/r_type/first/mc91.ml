(*
USED: PLDI2011 as mc91
USED: PEPM2013 as mc91

Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ mc91 :: x:Int -> {b:Int | (b == 91)} @-}
*)

let rec mc91 x =
  if x > 100
  then x - 10
  else mc91 (mc91 (x + 11))


let main (n:int) =
    if n <= 101
    then assert (mc91 n = 91)
    else assert(true)

let _ = main 12
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)