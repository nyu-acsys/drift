(*
USED: PLDI2011 as r-lock
USED: PEPM2013 as r-lock
*)

let lock st = assert (st = 0); 1 
let unlock st = assert (st = 1); 0 
let f n st = if n > 0 then lock (st) else st 
let g n st = if n > 0 then unlock (st) else st 
    
let main_p (mn:int) = 
    assert ((g mn (f mn 0)) = 0) 

let main (w:unit) =
	let _ = main_p 10 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()