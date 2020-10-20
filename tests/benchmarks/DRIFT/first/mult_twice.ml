(*
From: jayhorn/benchmarks MultCommutative01_true.java
*)

let rec mult (mn:int) (mm:int) = 
    if mn <= 0 || mm <= 0 then
   		0
  	else
   		mn + mult mn (mm - 1)
    
let main (n:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
    if m < 0 && n < 0 then ()
    else
        let res1 = mult m n in
        let res2 = mult n m in
        assert(res1 = res2)

(* let _ = 
    for i = 1 to 10000 do
      main (Random.int 1000) (Random.int 1000)
    done *)