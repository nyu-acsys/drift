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

let main (n:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
	if n < 0 && m < 0 then ()
	else
		loopa 0 1000 n n m m;
		loopb 0 100 n n

(* let _ = 
    for i = 1 to 10000 do
      main (Random.int 10000) (Random.int 10000)
    done *)