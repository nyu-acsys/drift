
let main (n(*-:{v:Int | v > 0}*)) = 
	let rec reverse ri rn ra rb =
		if (ri < rn) then
			let _ = set rb (rn - ri - 1) (get ra ri) in
			reverse (ri + 1) rn ra rb
		else ()
	in

	let a = make n 0 in
	let b = make n 0 in
	reverse 0 n a b