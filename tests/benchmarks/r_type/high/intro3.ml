(*
USED: PLDI2011 as intro3
*)


let f (x:int) (g:int -> bool) = g (x + 1)

let h (z:int) (y:int) = y > z

let main (n:int(*-:{v:Int | true}*)) =
	if n >= 0 then assert(f n (h n))
	else assert(true)

let _ = main 15