(*
USED: PLDI2011 as intro2
*)


let f x (g:int -> bool) = g (x + 1)

let h (y:int) = y > 0

let main (n:int(*-:{v:Int | true}*)) =
	if n >= 0 then assert(f n h)
	else ()