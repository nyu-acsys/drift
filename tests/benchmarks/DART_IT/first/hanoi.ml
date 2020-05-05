(*
From: jayhorn/benchmarks SatHanoi01_true.java
*)

let rec hanoi (hn:int) = 
	if hn = 1 then 1
	else 2 * (hanoi (hn - 1)) + 1

let rec apply_hanoi (an:int) (from:int) (too:int) (via:int) = 
	if an = 0 then 0
	else apply_hanoi (an - 1) from via too
		+ apply_hanoi (an - 1) via too from + 1

let main (n:int(*-:{v:Int | true}*)) = 
	if n < 1 || n > 31 then ()
	else 
		let res1 = apply_hanoi n 1 3 2 in
		let res2 = hanoi n in
		assert(res1 = res2)

(* let _ = 
    for i = 1 to 10 do
      main (Random.int 31)
    done *)