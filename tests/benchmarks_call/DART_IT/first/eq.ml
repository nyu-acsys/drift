(*
From: SV-COMP 2020 eq*.c
*)

let rec loopa (ai:int) (an:int) (ax:int) (aw:int) (az:int) (ay:int) = 
	if ai >= an then assert(ax = aw && az = ay)
	else if ai mod 2 = 0 then
		loopa (ai + 1) an (ax + 1) (aw + 1) az ay
		else loopa (ai + 1) an ax aw (az - 1) (ay - 1)

let rec loopb (bi:int) (bn:int) (by:int) (bz:int) = 
	if bi >= bn then assert(bz = by)
	else 
		loopb (bi + 1) bn (by + 1) (bz + 1)

let main_p (n:int) (m:int) =
	if n < 0 && m < 0 then ()
	else
		loopa 0 1000 n n m m;
		loopb 0 100 n n

let main (w:unit) =
	let _ = main_p 10 4 in
	let _ = main_p 30 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()