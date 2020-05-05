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
    
let main_p (n:int) = 
    if n <= 0 then ()
    else assert(loop 0 n 0 0 = 0)

let main (w:unit) =
	let _ = main_p 10 in
	let _ = main_p 30 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()