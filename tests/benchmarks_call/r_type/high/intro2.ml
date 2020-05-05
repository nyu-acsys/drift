(*
USED: PLDI2011 as intro2
*)


let f x (g:int -> bool) = g (x + 1)

let h (y:int) = y > 0

let main_p (n:int) =
	if n >= 0 then assert(f n h)
	else ()

let main (w:unit) =
	let _ = main_p 0 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()