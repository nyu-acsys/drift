(*
Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise 0 in
  try ff ?n? with Failer 0 -> ()
*)


let f fn fk = if fn >= 0 then true else fk 0
let g gn = gn = 0

let main n = 
	assert (f n g)

let _ = main (-5)
