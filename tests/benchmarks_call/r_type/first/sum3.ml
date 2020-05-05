(*
Res: OCaml -> 42813631
imprecision: Oct: 9253 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)

let main_p (mn:int) =
    assert (3 * mn - 3 <= sum mn)

let main (w:unit) =
	let _ = main_p 92 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()

(* Reason: wid overapproximate *)