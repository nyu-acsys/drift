  
(*
 * Recursive implementation of the greatest common denominator
 * using Euclid's algorithm
 * 
 * Author: Jan Leike
 * Date: 2013-07-17
 * 
 *)


(* Compute the greatest common denominator using Euclid's algorithm *)

let rec gcd y1 y2 =
    if (y1 <= 0 || y2 <= 0) then 0
    else if (y1 = y2) then y1
    else if (y1 > y2) then
        gcd (y1 - y2) y2
    else gcd y1 (y2 - y1)

let main_p (m:int) (n:int) =
    if m > 0 && n > 0 then 
        let res = gcd m n in
        assert(res <= m && res <= n)
    else ()

let main (w:unit) =
	let _ = main_p 12 24 in
    let _ = main_p 144 120 in
    let _ = main_p 0 0 in
    let _ = main_p (-45) (-30) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()