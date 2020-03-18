(*
USED: PLDI2011 as hrec
*)

let main (n(*-:{v:Int | true}*)) = 
	let rec f fg fx = if fx >= 0 then fg fx else f (f fg) (fg fx) in
	let succ sx = sx + 1 in
	assert (f succ n >= 0)
(* in main (-44) *)