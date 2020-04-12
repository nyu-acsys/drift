(*
Drop every i'th element from 
*)

let main (n(*-:{v:Int | v = 30}*)) (i(*-:{v:Int | v = 4}*)) = 
	let rec drop cai can cbi cbn cdi cdn ca cb = 
		if cai < can && cbi < cbn then 
			if cdi < cdn then
				(set cb cbi (get ca cai);
				drop (cai + 1) can (cbi + 1) cbn (cdi + 1) cdn ca cb)
			else drop (cai + 1) can cbi cbn 0 cdn ca cb
		else cb
	in

	let a = make n 0 in
	let la = len a in
	let res = 
		if i < n then
			let j = n - (n - (n mod i)) / i in
			let b = make j 0 in
			let lb = len b in
			let len1 = len (drop 0 la 0 lb 0 i a b) in
			len1
		else
			la
	in
	assert(res <= la)