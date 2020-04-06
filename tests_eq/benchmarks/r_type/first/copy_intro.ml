(*
    USED: PEPM2013 as copy_intro
*)


let rec copy x = 
    if x = 0 then 0 
    else 1 + copy (x - 1) 

let main (n:int) =
    assert (copy (copy n) = n) 

let _ = main 1823
let _ = main 0
let _ = main (-4)