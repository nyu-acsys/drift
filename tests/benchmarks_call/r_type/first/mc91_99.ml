
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v <= 101
Liquid Haskell: false by property {-@ m :: x:Int -> {b:Int | (b == 91)} @-}
*)

let rec m x =
  if x > 100
  then x - 10
  else m (m (x + 11))

let main_p (n:int) =
    if n <= 99
    then assert (m n = 91)
    else ()

let main (w:unit) =
	let _ = main_p 1 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()