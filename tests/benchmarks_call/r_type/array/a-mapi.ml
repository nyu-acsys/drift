
let make_array (n:int) (i:int) = assert (0 <= i && i < n); 0
let update (i:int) (n:int) a (x:int) = 
	a i;
  	let ap j = assert (0 <= i && i < n); if i = j then x else a j in ap

let rec mapi_helper (hf: int -> int -> int) hi hn (ha: int -> int) (hb: int -> int) : int -> int = 
	if (hi < hn) then
		let hb2 = update hi hn hb (hf hi (ha(hi))) in
		mapi_helper hf (hi + 1) hn ha hb2
	else hb

let mapi (mf: int -> int -> int) ma mn : int -> int =
	let mb = make_array mn in
	mapi_helper mf 0 mn ma mb

let add_idx (sidx: int) (si: int) = sidx + si

let main_p (n:int) (k:int) =
	let a = make_array n in
	let b = mapi add_idx a n in
	if k >= 0 && k < n then
		(b(k); ())
	else ()

let main (w:unit) =
	let _ = main_p 5 2 in
	let _ = main_p 19 3 in
	(* let _ = 
			for i = 1 to 1000 do
				main_p (Random.int 1000) (Random.int 1000)
			done in *)
	()

let _ = main ()