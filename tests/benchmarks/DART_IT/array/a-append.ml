
let rec append_helper hi hn hla (ha: int array) (hb: int array) (hc: int array) = 
	if (hi < hn) then
		let _ = 
			if hi < hla then
				Array.set hc hi (Array.get ha hi)
			else
				Array.set hc hi (Array.get hb (hi - hla))
		in
		append_helper (hi + 1) hn hla ha hb hc
	else ()

let append (ma: int array) (mb: int array) =
	let la = Array.length ma in
	let lb = Array.length mb in
	let mn = la + lb in
	let mc = Array.make mn 0 in
	append_helper 0 mn la ma mb mc;
	Array.length mc

let main (n:int(*-:{v:Int | true}*)) =
	let res: bool = 
		if n > 0 then
			let a = Array.make n 0 in
			let b = Array.make n 0 in
			let la = Array.length a in
			let lb = Array.length b in
			let lc = append a b in
			lc = la + lb
		else true
	in assert (res = true)

let _ = main 5