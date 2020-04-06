
let main (n(*-:{v:Int | v > 0}*)) = 
	let succ si = si + 1 in

	let rec iter_helper hf hi hn ha = 
		if (hi < hn) then
			let _ = hf (get ha hi) in
			iter_helper hf (hi + 1) hn ha
		else ()
	in

	let rec iter mf ma =
		let mn = len ma in
		iter_helper mf 0 mn ma
	in

	let a = make n 0 in
	iter succ a