(*
USED: PLDI2011 as repeat
*)


let succ sx = sx + 1

let rec repeat (rf: int -> int) rn rs =
  if rn = 0 then
    rs
  else
    rf (repeat rf (rn - 1) rs)

let main_p (n:int) =
	assert (repeat succ n 0 = n)

let main (w:unit) =
	let _ = main_p 103 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()