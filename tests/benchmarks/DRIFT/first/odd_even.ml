(*
Revised From: jayhorn/benchmarks SatHanoi01_true.java
*)

let rec is_odd on =
	if on = 0 then false
	else if on = 1 then true
	else is_odd (on - 2)

let rec is_even en =
	if en = 0 then true
	else if en = 1 then false
	else is_even (en - 2)

let main (n:int(*-:{v:Int | true}*)) = 
    if n > 0 then
        if n mod 2 = 0 then 
        	assert(is_even n)
        else if n mod 2 = 1 then
        	   assert(is_odd n)
            else ()
    else ()

(* let _ = 
    for i = 1 to 10000 do
      main (Random.int 1000)
    done *)