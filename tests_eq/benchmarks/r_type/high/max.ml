(*
USED: PLDI2011 as max
USED: PEPM2013 as max
*)


let max max2 (mx:int) (my:int) (mz:int) = max2 (max2 mx my) mz
let f (fx:int) (fy:int) = if fx >= fy then fx else fy

let main (x:int) (y:int) (z:int) =
    let m = max f x y z in
    assert (f x m = m)

let _ = main 3 4 5
let _ = main 10 1 2
let _ = main 0 2 4
let _ = main 3 0 5
let _ = main 54 23 0
let _ = main (-34) 75 (-94)
