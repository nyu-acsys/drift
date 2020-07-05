
let rec mapfilter (f:int->int list) (l:int list) = match l with
	| [] -> []
	| h::t -> 
		let r = mapfilter f t in
		let x = f h in
		match x with
			| [] -> r
			| z::e -> (z::r)		

let pos y =
  if 0 < y then
    y::[]
  else
    []

let main (u:unit(*-:{v:Unit | unit}*)) = 
	let xs = [1;2;1] in
	let ys = mapfilter pos xs in
	assert(List.length ys = List.length xs)