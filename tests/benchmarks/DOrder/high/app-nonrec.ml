

let apply (ab:int) (af: int->unit) ax = af ax

let check (cx:int) (cy:int) = assert (cx = cy)

let main (n:int(*-:{v:Int | true}*)) =
	apply n (check n) n
