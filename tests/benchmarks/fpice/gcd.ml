  
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
in

let main k =
    let m = 24 in
    if (m <= 0 || m > 2000) then 0
   	else
    	let n = 18 in
    	if (n <= 0 || n > 2000) then 0
			else
      	let z = gcd m n in
    		if (z < 1 && m > 0 && n > 0) then
					-1
				else z
in
assert(main () = 6)