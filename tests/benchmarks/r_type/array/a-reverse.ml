
let make_array n i = assert (0 <= i && i < n); 0
let update i (n:int) a x j = assert (0 <= i && i < n); if j = i then x else a(j)

let rec reverse ri rn ra rb =
	if (ri < rn) then
		let rb2 = update (rn - ri - 1) rn rb (ra(ri)) in
		reverse (ri + 1) rn ra rb2
	else ()

let main (n:int) = 
	let a = make_array n in
	let b = make_array n in
	reverse 0 n a b