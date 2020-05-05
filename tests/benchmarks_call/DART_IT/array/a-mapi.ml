

let rec mapi_helper (hf: int -> int -> int) hi hn (ha: int array) (hb: int array) = 
	if (hi < hn) then
		let _ = Array.set hb hi (hf hi (Array.get ha hi)) in
		mapi_helper hf (hi + 1) hn ha hb
	else hb

let rec mapi (mf: int -> int -> int) (ma: int array) =
	let mn = Array.length ma in
	let mb = Array.make mn 0 in
	mapi_helper mf 0 mn ma mb

let add_idx sidx si = sidx + si

let main_p (n:int) =
	if n > 0 then
		let a = Array.make n 0 in
		assert(Array.length (mapi add_idx a) = Array.length a)
	else ()

let main (w:unit) =
	let _ = main_p 5 in
	let _ = main_p 19 in
	()

let _ = main ()