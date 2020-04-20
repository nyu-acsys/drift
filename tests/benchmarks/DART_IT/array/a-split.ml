
let rec copy cai can cbi cbn (ca: int array) (cb: int array) = 
	if cai < can && cbi < cbn then 
		(Array.set cb cbi (Array.get ca cai);
		copy (cai + 1) can (cbi + 1) cbn ca cb)
	else ()

let main (n(*-:{v:Int | true}*)) (i(*-:{v:Int | true}*)) = 
	let ans: bool = 
		if n > 0 then
			let a = Array.make n 0 in
			let la = Array.length a in
			let res = 
				if i >= 0 && i < n then
					let b = Array.make i 0 in
					let lb = Array.length b in
					copy 0 la 0 lb a b;
					let c = Array.make (la - i) 0 in
					let lc = Array.length c in
					copy i la 0 lc a c;
					lb + lc
				else
					la
			in
			res = la
		else true
	in assert (ans = true)

let _ = main 10 4