
let main (n(*-:{v:Int | v >= 0}*)) (start(*-:{v:Int | v >= 0}*)) (subl(*-:{v:Int | v >= 0}*)) = 
	let rec sub_helper hi hstart hn hlen ha hb = 
		if (hlen > 0 && hi < hlen) then
			(set hb hi (get ha hstart);
			sub_helper (hi + 1) (hstart + 1) hn hlen ha hb)
		else hb
	in

	let rec sub ma mstart mlen =
		let la = len ma in
		let mb = make mlen 0 in
		sub_helper 0 mstart la mlen ma mb
	in

	let a = make n 0 in
	let la = len a in
	let res = 
		if subl >= 0 && start >= 0 && start + subl < la then
			sub a start subl
		else
			a
	in
	assert (len res <= len a)