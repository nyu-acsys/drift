
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v
Liquid Haskell: TODO: need to TEST!
*)


let rec m mx (mk:int -> bool): bool =
  if mx > 100
  then true && mk (mx - 10)
  else
    let f (fr:int) = m fr mk in
    m (mx + 11) f

let main_p (n:int) =
	if n <= 101 then
	    (let k kr = (kr = 91) in
	    assert (m n k = true))
	else ()

let main (w:unit) =
	let _ = main_p 22 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
