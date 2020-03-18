(*
USED: PLDI2011 as e-simpl
USED: PEPM2013 as e-simpl
FROM: Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise (Failer 0)
let main n = try ff n with Failer 0 -> ()
*)

let main (n(*-:{v:Int | true}*)) = 
    let f fn fk = if fn >= 0 then assert(true) else fk 0 in
    let g gn = assert (gn = 0) in
    f n g
(* in
main (-3) *)
