(*
USED: PLDI2011 as hrec
*)


let rec f (fg: int -> int) fx = if fx >= 0 then fg fx else f (f fg) (fg fx)
let succ sx = sx + 1

let main (n:int(*-:{v:Int | true}*)) = 
	assert (f succ n >= 0)