  
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

let main (m:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
    if m > 0 && n > 0 then 
        let res = gcd m n in
        assert(res <= m && res <= n)
    else ()