(*
USED: PLDI2011 as intro1
*)


let f x g = g (x + 1)

let h y = y > 0

let main n = 
    if n > 0 then assert (f n h)

let _ = main 120