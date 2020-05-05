
let succ si = si + 1

let rec iter_helper (hf: int -> int) hi hn (ha: int array) = 
	if (hi < hn) then
		let _ = hf (Array.get ha hi) in
		iter_helper hf (hi + 1) hn ha
	else ()

let rec iter (mf: int -> int) (ma: int array) =
	let mn = Array.length ma in
	iter_helper mf 0 mn ma

let main_p (n:int) = 
	if n > 0 then
		let a = Array.make n 0 in
		iter succ a
	else ()

let main (w:unit) =
	let _ = main_p 30 in
	let _ = main_p 10 in
	()

let _ = main ()