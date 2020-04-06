

let main (mx(*-:{v:Int | true}*)) = 
	let mf fx = fx + 5 in

	let mg gx = 2 * gx in

	let mh hx = hx - 5 in

	let compose_1 f1 g1 x1 = f1 (g1 x1) in

	let compose_2 f2 g2 x2 = g2 (f2 x2) in

	let ans1 = mh (compose_1 mf mg mx) = 2 * mx in
	let ans2 = mf (compose_2 mg mh mx) = 2 * mx in
	assert(ans1 && ans2 = true)

(* let _ = main 10
let _ = main (-2)
let _ = main 0
let _ = main 200
let _ = main (-443) *)
