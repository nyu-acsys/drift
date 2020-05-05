(*
From: jayhorn/benchmarks SatAddition01_true.java
*)

let rec addition (lm:int) (ln:int) =
    if ln = 0 then lm
    else if ln > 0 then
        addition (lm + 1) (ln - 1)
    else addition (lm - 1) (ln + 1)
    
let main_p (m:int) (n:int) = 
    if n > 0 && m > 0 then
        assert(addition m n = m + n)
    else ()
    
let main (w:unit) =
	let _ = main_p 10 4 in
	let _ = main_p 30 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()