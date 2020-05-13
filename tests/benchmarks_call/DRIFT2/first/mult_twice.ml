(*
From: jayhorn/benchmarks MultCommutative01_true.java
*)

let rec mult (mn:int) (mm:int) = 
    if mn <= 0 || mm <= 0 then
   		0
  	else
   		mn + mult mn (mm - 1)
    
let main_p (n:int) (m:int) =
    if m < 0 && n < 0 then ()
    else
        let res1 = mult m n in
        let res2 = mult n m in
        assert(res1 = res2)

let main (w:unit) =
	let _ = main_p 3 4 in
	let _ = main_p 7 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()