(*
USED: PLDI2011 as repeat
*)


let succ sx = sx + 1

let rec repeat (rf: int -> int) rn rs =
  if rn = 0 then
    rs
  else
    rf (repeat rf (rn - 1) rs)

let main (n:int(*-:{v:Int | true}*)) =
	assert (repeat succ n 0 = n)
