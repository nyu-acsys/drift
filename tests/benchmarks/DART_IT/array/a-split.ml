

let main (n(*-:{v:Int | v >= 0}*)) (i(*-:{v:Int | v >= 0}*)) = 
	let rec copy cai can cbi cbn ca cb = 
		if cai < can && cbi < cbn then 
			(set cb cbi (get ca cai);
			copy (cai + 1) can (cbi + 1) cbn ca cb)
		else cb
	in

	let a = make n 0 in
	let la = len a in
	let res = 
		if i < n then
			let b = make i 0 in
			let lb = len b in
			let len1 = len (copy 0 la 0 lb a b) in
			let c = make (la - i) 0 in
			let lc = len c in
			let len2 = len (copy i la 0 lc a c) in
			len1 + len2
		else
			la
	in
	assert (res = la)