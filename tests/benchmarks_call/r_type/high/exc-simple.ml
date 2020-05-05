(*
USED: PLDI2011 as e-simpl
USED: PEPM2013 as e-simpl
FROM: Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise (Failer 0)
let main n = try ff n with Failer 0 -> ()
*)

let f fn (fk: int -> unit) = if fn >= 0 then assert(true) else fk 0
let g gn = assert (gn = 0)

let main_p (n:int) = 
    f n g

let main (w:unit) =
	let _ = main_p (-3) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
