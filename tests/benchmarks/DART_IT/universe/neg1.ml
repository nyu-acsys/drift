

let main (n(*-:{v:Int | true}*)) =
	let twice f y = f (f y) in

	let neg z = 0 - z in

	if (n >= 0) then
	   let v = twice neg (2 * n) in
	   assert (v >= 0)
	else assert(true)


