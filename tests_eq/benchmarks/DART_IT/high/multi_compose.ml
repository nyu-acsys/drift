

let mf (fx:int) = fx + 5

let mg (gx:int) = 2 * gx

let mh (hx:int) = hx - 5

let compose_1 (f1:int -> int) (g1:int -> int) (x1:int) = f1 (g1 x1)

let compose_2 (f2:int -> int) (g2:int -> int) (x2:int) = g2 (f2 x2)

let main mx = 
	let ans1 = mh (compose_1 mf mg mx) = 2 * mx in
	let ans2 = mf (compose_2 mg mh mx) = 2 * mx in
	assert(ans1 && ans2 = true)

let _ = main 10
let _ = main (-2)
let _ = main 0
let _ = main 200
let _ = main (-443)
