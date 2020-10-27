
let rec sub_helper hi hstart hn hlen (ha: int array) (hb: int array) = 
	if (hlen > 0 && hi < hlen) then
		(Array.set hb hi (Array.get ha hstart);
		sub_helper (hi + 1) (hstart + 1) hn hlen ha hb)
	else ()

let rec sub (ma: int array) mstart mlen =
	let la = Array.length ma in
	let mb = Array.make mlen 0 in
	sub_helper 0 mstart la mlen ma mb;
	mb

let main (n(*-:{v:Int | true}*)) (start(*-:{v:Int | true}*)) (subl(*-:{v:Int | true}*)) = 
	if n > 0 then
		let a = Array.make n 0 in
		let res: int array =
			let la = Array.length a in
			if subl >= 0 && start >= 0 && start + subl < la then
				sub a start subl
			else
				a
		in
		assert(Array.length res <= Array.length a)
	else ()