(*
Res: OCaml -> 284635
imprecision: Oct: 754 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)

let main_p (mn:int) =
    assert (4 * mn - 6 <= sum mn)

let main (w:unit) =
	let _ = main_p 754 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()

(* Reason: wid overapproximate *)