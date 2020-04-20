

let rec mapi_helper (hf: int -> int -> int) hi hn (ha: int array) (hb: int array) = 
	if (hi < hn) then
		let _ = Array.set hb hi (hf hi (Array.get ha hi)) in
		mapi_helper hf (hi + 1) hn ha hb
	else ()

let rec mapi (mf: int -> int -> int) (ma: int array) =
	let mn = Array.length ma in
	let mb = Array.make mn 0 in
	mapi_helper mf 0 mn ma mb;
	Array.length mb = Array.length ma

let add_idx sidx si = sidx + si

let main (n:int(*-:{v:Int | true}*)) =
	let ans: bool = if n > 0 then
		let a = Array.make n 0 in
		mapi add_idx a
	else true
	in assert(ans = true)

let _ = main 5