
let make_array (n:int) (i:int) = assert (0 <= i && i < n); 0
let update (i:int) (n:int) a (x:int) = 
	a i;
  	let ap j = assert (0 <= i && i < n); if i = j then x else a j in ap

let rec sub_helper hi hstart hn hlen ha hb = 
	if (hlen > 0 && hi < hlen) then
		(let hbp = update hi hlen hb (ha hstart) in
		sub_helper (hi + 1) (hstart + 1) hn hlen ha hbp)
	else hb

let rec sub ma mstart mlen mn =
	let mb = make_array mlen in
	sub_helper 0 mstart mn mlen ma mb

let main_p (n:int) (start:int) (subl:int) (k:int) =
	let a = make_array n in
	if subl >= 0 && start >= 0 && start + subl < n then
		(let b = sub a start subl in
		if k >= 0 && k < subl then
			(b(k); ())
		else ())
	else
		()

let main (w:unit) =
	let _ = main_p 10 3 4 2 in
    let _ = main_p 100 30 50 4 in
		(* let _ = 
			for i = 1 to 1000 do
				main_p (Random.int 1000) (Random.int 1000) (Random.int 1000) (Random.int 1000)
			done in *)
	()

let _ = main ()