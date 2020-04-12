(*
Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise 0 in
  try ff ?n? with Failer 0 -> ()
*)

let main (n(*-:{v:Int | true}*)) = 
	let f fn fk = if fn >= 0 then true else fk 0 in
	let g gn = gn = 0 in
	assert (f n g)
(* in
main (-5) *)
