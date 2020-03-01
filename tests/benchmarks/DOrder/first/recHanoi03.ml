  
(*
 * recHanoi.c
 *
 *  Created on: 17.07.2013
 *      Author: Stefan Wissert
 *)


(*
 * This function returns the optimal amount of steps,
 * needed to solve the problem for n-disks
 *)
let rec hanoi n =
	if (n = 1) then 1
    else 2 * (hanoi (n - 1)) + 1
in

let main k =
    let n = 13 in
    if (n < 1 || n > 31) then false
   	else
    	let result = hanoi n in
		assert (result >= n)
in
main ()	