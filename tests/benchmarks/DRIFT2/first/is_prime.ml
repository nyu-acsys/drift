(*
From: jayhorn/benchmarks SatPrimes01_true.java
*)

let rec mult (mn:int) (mm:int) = 
    if mm < 0 then 
        mult mn (0 - mm)
    else if mm = 0 then 0
    else mn + (mult mn (mm - 1))

let rec multiple_of (on:int) (om:int) = 
	if om < 0 then multiple_of on (0 - om)
	else if on < 0 then multiple_of (0 - on) om
	else if om = 0 then false
	else if on = 0 then true
	else multiple_of (on - om) om

let rec is_prime_helper (hn:int) (hm:int) = 
	if hn <= 1 then false
	else if hn = 2 then true
	else if hm <= 1 then true
	else if multiple_of hn hm = false then false
	else is_prime_helper hn (hm - 1)

let is_prime (i:int) = 
	is_prime_helper i (i - 1)

let main (k:unit(*-:{v:Unit | unit}*))  =
	let n = 7 in
	let f1 = 3 in
	let f2 = 2 in 
	if n < 1 || n > 46340 then ()
	else if f1 <= 1 || f1 > 46340 then ()
	else if f2 <= 1 || f2 > 46340 then ()
	else 
		let res = is_prime n in
		if res then assert(mult f1 f2 + 1 = n)
		else ()

(* let _ = main () *)