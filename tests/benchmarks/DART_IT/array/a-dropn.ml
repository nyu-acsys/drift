(*
Drop every i'th element from 
*)

let rec drop cai can cbi cbn cdi cdn (ca: int array) (cb: int array) = 
	if cai < can && cbi < cbn then 
		if cdi < cdn then
			(Array.set cb cbi (Array.get ca cai);
			drop (cai + 1) can (cbi + 1) cbn (cdi + 1) cdn ca cb)
		else drop (cai + 1) can cbi cbn 0 cdn ca cb
	else ()

let main (n(*-:{v:Int | v = 30}*)) (i(*-:{v:Int | v = 4}*)) = 
	if n > 0 then
		let a = Array.make n 0 in
		let la = Array.length a in
		let res:int = 
			if i < n then
				let j = n - (n - (n mod i)) / i in
				let b = Array.make j 0 in
				let lb = Array.length b in
				drop 0 la 0 lb 0 i a b;
				lb
			else
				la
		in
		assert (res <= la)
	else ()