

let apply (ab:int) (af: int->unit) ax = af ax

let check (cx:int) (cy:int) = assert (cx = cy)

let main_p (n:int) =
	apply n (check n) n

let main (w:unit) =
	let _ = main_p 2 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()