
let rec map_helper (hf: int -> int) hi hn (ha: int array) (hb: int array) = 
	if (hi < hn) then
		let _ = Array.set hb hi (hf (Array.get ha hi)) in
		map_helper hf (hi + 1) hn ha hb
	else ()

let rec map (mf: int -> int) (ma: int array) =
	let mn = Array.length ma in
	let mb = Array.make mn 0 in
	map_helper mf 0 mn ma mb;
	Array.length mb = Array.length ma

let succ si = si + 1

let main (n:int(*-:{v:Int | true}*)) =
	let ans: bool = if n > 0 then
		let a = Array.make n 0 in
		map succ a
	else true
	in assert(ans = true)

let _ = main 10