

let rec reverse ri rn (ra: int array) (rb: int array) =
	if (ri < rn) then
		let _ = Array.set rb (rn - ri - 1) (Array.get ra ri) in
		reverse (ri + 1) rn ra rb
	else ()

let main (n(*-:{v:Int | true}*)) = 
	if n > 0 then
		let a = Array.make n 0 in
		let b = Array.make n 0 in
		reverse 0 n a b
	else ()