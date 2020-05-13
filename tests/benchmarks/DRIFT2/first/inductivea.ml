(*
From: jayhorn/benchmarks TestLoop2_true.java and TestLoop3_true.java
*)

let rec loop (lx:int) (ly:int) = 
	if lx = 0 then ly
	else loop (lx - 1) (ly - 1)

let rec loopb (bx:int) (by:int) = 
	if bx < by then loopb (bx + by) by
	else assert(bx >= by)

let main (i:int(*-:{v:Int | true}*)) (j:int(*-:{v:Int | true}*)) =
	if i < 0 && j < 0 then ()
	else
		let x = i in
		let y = j in
		assert(loop x y + i <= j);
		loopb i j

(* let _ = 
    for i = 1 to 10000 do
      main (Random.int 1000) (Random.int 1000)
    done *)