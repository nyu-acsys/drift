
let succ si = si + 1

let rec iter_helper (hf: int -> int) hi hn (ha: int array) = 
	if (hi < hn) then
		let _ = hf (Array.get ha hi) in
		iter_helper hf (hi + 1) hn ha
	else ()

let rec iter (mf: int -> int) (ma: int array) =
	let mn = Array.length ma in
	iter_helper mf 0 mn ma

let main (n(*-:{v:Int | true}*)) = 
	if n > 0 then
		let a = Array.make n 0 in
		iter succ a
	else ()

let _ = main 30