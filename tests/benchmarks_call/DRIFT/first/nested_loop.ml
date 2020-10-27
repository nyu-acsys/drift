(*
From: SV-COMP 2020 nested-1.c
https://github.com/sosy-lab/sv-benchmarks/tree/master/c/loop-new
*)

let rec loopa (aj:int) (am:int) (ak:int) = 
	if aj >= am then ak
	else loopa (aj + 1) am (ak + 1)

let rec loopb (bi:int) (bn:int) (bm:int) (bk:int) = 
	if bi >= bn then bk
	else 
		let rk = loopa 0 bm bk in
		loopb (bi + 1) bn bm rk

let main_p (n:int) (m:int) =
	if n < 10 || n > 10000 then ()
	else if m < 10 || m > 10000 then ()
	else
		assert(loopb 0 n m 0 >= 100)

let main (w:unit) =
	let _ = main_p 20 40 in
	let _ = main_p 70 100 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()