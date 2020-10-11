(*
USED: PLDI2011 as intro3
*)


let f (x:int) g: unit  = g (x + 1)

let h (z:int) (y:int) = assert(y > z)

let main (n:int(*-:{v:Int | true}*)) =
	if n >= 0 then f n (h n)