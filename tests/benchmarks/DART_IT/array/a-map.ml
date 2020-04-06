

let main (n(*-:{v:Int | v > 0}*)) = 
	let rec map_helper hf hi hn ha hb = 
		if (hi < hn) then
			let _ = set hb hi (hf (get ha hi)) in
			map_helper hf (hi + 1) hn ha hb
		else hb
	in

	let rec map mf ma =
		let mn = len ma in
		let mb = make mn 0 in
		map_helper mf 0 mn ma mb
	in

	let succ si = si + 1 in

	let a = make n 0 in
	let b = map succ a in
	assert(len b = len a)