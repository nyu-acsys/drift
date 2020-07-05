let rec make_list m =
	if m <= 0 then []
	else m :: make_list (m - 1)

let f xs = match xs with
	| [] -> ()
	| x :: xs -> assert (x > 0)

let main (n:int(*-:{v:Int | true}*)) =
	let xs = make_list n in
	f xs