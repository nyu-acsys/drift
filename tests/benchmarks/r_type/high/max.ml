(*
USED: PLDI2011 as max
USED: PEPM2013 as max
*)


let max (max2: int -> int -> int) (mx:int) (my:int) (mz:int) = max2 (max2 mx my) mz
let f (fx:int) (fy:int) = if fx >= fy then fx else fy

let main (x:int(*-:{v:Int | true}*)) (y:int(*-:{v:Int | true}*)) (z:int(*-:{v:Int | true}*)) =
    let m = max f x y z in
    assert (f x m = m)