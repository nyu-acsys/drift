(*
USED: PLDI2011 as max
USED: PEPM2013 as max
*)

let main x y z =
    let max max2 mx my mz = max2 (max2 mx my) mz in
    let f fx fy = if fx >= fy then fx else fy in

    let m = max f x y z in
    assert (f x m = m)
in main 3 4 5
