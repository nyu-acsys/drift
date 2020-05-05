(*
USED: PLDI2011 as intro3
*)


let f (x:int) (g:int -> bool) = g (x + 1)

let h (z:int) (y:int) = y > z

let main_p (n:int) =
	if n >= 0 then assert(f n (h n))
	else ()

let main (w:unit) =
	let _ = main_p 15 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()