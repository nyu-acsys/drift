(*
From: SV-COMP 2020 for_bounded_loop1.c
*)

let rec loop (i:int) (n:int) (x:int) (y:int) =
    if i >= n then x - y
    else
        let xp = x - y in
        assert(xp = 0);
        let yp = y + i in
        loop (i + 1) n (xp + yp) (yp)
    
let main (n:int(*-:{v:Int | true}*)) = 
    if n <= 0 then ()
    else assert(loop 0 n 0 0 = 0)

(* let _ = 
    for i = 1 to 10000 do
      main (Random.int 1000)
    done *)