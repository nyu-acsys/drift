(*
Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise 0 in
  try ff ?n? with Failer 0 -> ()
*)


let f fn (fk:int -> bool) = if fn >= 0 then true else fk 0
let g gn = gn = 0

let main_p (n:int) = 
	assert (f n g)

let main (w:unit) =
	let _ = main_p (-5) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()