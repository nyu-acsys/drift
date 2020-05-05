(*
USED: PLDI2011 as hrec
*)


let rec f (fg: int -> int) fx = if fx >= 0 then fg fx else f (f fg) (fg fx)
let succ sx = sx + 1

let main_p (n:int) = 
	assert (f succ n >= 0)

let main (w:unit) =
	let _ = main_p (-44) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()