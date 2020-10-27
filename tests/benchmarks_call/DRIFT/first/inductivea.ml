(*
From: jayhorn/benchmarks TestLoop2_true.java and TestLoop3_true.java
*)

let rec loop (lx:int) (ly:int) = 
	if lx = 0 then ly
	else loop (lx - 1) (ly - 1)

let rec loopb (bx:int) (by:int) = 
	if bx < by then loopb (bx + by) by
	else assert(bx >= by)

let main_p (i:int) (j:int) =
	if i < 0 && j < 0 then ()
	else
		let x = i in
		let y = j in
		assert(loop x y + i <= j);
		loopb i j

let main (w:unit) =
	let _ = main_p 30 20 in
	let _ = main_p 5 4 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()