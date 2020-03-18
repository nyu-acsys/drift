(*
USED: PLDI2011 as r-lock
USED: PEPM2013 as r-lock
*)


let lock st = assert (st = 0); 1 
let unlock st = assert (st = 1); 0 
let f n st = if n > 0 then lock (st) else st 
let g n st = if n > 0 then unlock (st) else st 
    
let main mn = 
    assert ((g mn (f mn 0)) = 0) 

let _ = main 10
