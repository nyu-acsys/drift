
let rec f (g: int -> int) x = if x >= 0 then g x else f (f g) (g x)
let succ sx = sx + 1

let main (n:int(*-:{v:Int | true}*)) =
	assert (f succ n >= 0)