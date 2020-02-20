

let mf fx = fx + 1 in

let mg gx = 2 * gx in

let mh hx = hx - 3 in

let compose f g x = f (g x) in

let main mx = 
	let ans1 = compose mf mg mx in
	let ans2 = compose mh mf ans1 in
	assert(compose mg mh ans2 = 32)
in main 10