
let f x z = 
	if z then x else 2 in

let id k = k in 

let main (n(*-:{v:Int | top}*)) (m(*-:{v:Bool | top}*)) = 
	let res = f (id n) m in
	res
