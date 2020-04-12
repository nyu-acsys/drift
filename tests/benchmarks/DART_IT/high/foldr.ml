
let main (n(*-:{v:Int | true}*)) = 
	let rec reduce rf rn ru = (* foldr *)
	    if rn = 0 then ru
	    else rf rn (reduce rf (rn - 1) ru)
	in

	(* let rec reduce_neg nf nn nu = (* foldr *)
	    if nn = 0 then nu
	    else nf nn (reduce_neg nf (nn - 1) nu)
	in *)

	let gt_5 gk gt = if gk > 5 then 5 else gt
	in

	let not_bool nx = if nx then false else true in

	let exor ak at = (*at will always be true*)
	    let pk = not_bool at
	    in (pk || at) && (not_bool(pk && at)) (*XOR: given one ture will be true*)
	in

	if n >= 0 then
		assert(reduce gt_5 n 5 = 5)
	else
		let m = 0 - n in
		assert(reduce exor m true = true)

(* let _ = main 20
let _ = main 30
let _ = main 3098
let _ = main 0
let _ = main (-1)
let _ = main (-20)
let _ = main (-304) *)

(*
9 / 2 = bot?
assert(reduce (fun k t -> (k - k/2) = k/2) n true = ((n - n/2) = n/2))
*)