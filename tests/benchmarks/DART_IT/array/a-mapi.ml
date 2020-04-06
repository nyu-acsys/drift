
let main (n(*-:{v:Int | v > 0}*)) = 
	let rec mapi_helper hf hi hn ha hb = 
		if (hi < hn) then
			let _ = set hb hi (hf hi (get ha hi)) in
			mapi_helper hf (hi + 1) hn ha hb
		else hb
	in

	let rec mapi mf ma =
		let mn = len ma in
		let mb = make mn 0 in
		mapi_helper mf 0 mn ma mb
	in

	let add_idx sidx si = sidx + si in

	let a = make n 0 in
	let b = mapi add_idx a in
	assert(len b = len a)