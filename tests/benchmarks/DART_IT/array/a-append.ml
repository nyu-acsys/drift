
let main (n(*-:{v:Int | v > 0}*)) = 
	let rec append_helper hi hn hla ha hb hc = 
		if (hi < hn) then
			let _ = 
				if hi < hla then
					set hc hi (get ha hi)
				else
					set hc hi (get hb (hi - hla))
			in
			append_helper (hi + 1) hn hla ha hb hc
		else hc
	in

	let rec append ma mb =
		let la = len ma in
		let lb = len mb in
		let mn = la + lb in
		let mc = make mn 0 in
		append_helper 0 mn la ma mb mc
	in

	let a = make n 0 in
	let b = make n 0 in
	assert (len (append a b) = (len a) + (len b))