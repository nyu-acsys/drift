
let make_array (n:int) (i:int) = assert (0 <= i && i < n); 0

let rec iter_helper (hf: int -> int) hi hn (ha: int -> int) : unit = 
	if (hi < hn) then
		let _ = hf (ha(hi)) in
		iter_helper hf (hi + 1) hn ha
	else ()

let iter (mf: int -> int) ma mn : unit =
	iter_helper mf 0 mn ma

let succ (si:int) = si + 1

let main_p (n:int) =
	if n > 0 then
		let a = make_array n in
		iter succ a n
	else ()

let main (w:unit) =
	let _ = main_p 30 in
	let _ = main_p 10 in
	(* let _ = 
			for i = 1 to 1000 do
				main_p (Random.int 1000)
			done in *)
	()

let _ = main ()