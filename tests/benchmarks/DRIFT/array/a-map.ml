
let rec map_helper (hf: int -> int) hi hn (ha: int array) (hb: int array) = 
	if (hi < hn) then
		let _ = Array.set hb hi (hf (Array.get ha hi)) in
		map_helper hf (hi + 1) hn ha hb
	else hb

let rec map (mf: int -> int) (ma: int array) =
	let mn = Array.length ma in
	let mb = Array.make mn 0 in
	map_helper mf 0 mn ma mb

let succ si = si + 1

let main (n:int(*-:{v:Int | true}*)) =
	if n > 0 then
		let a = Array.make n 0 in
		assert(Array.length (map succ a) = Array.length a)
	else ()