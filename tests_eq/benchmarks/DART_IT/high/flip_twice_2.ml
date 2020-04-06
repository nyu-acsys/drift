


let twice tf (tx:int) (ty:int) = tf (tf tx ty) ty

let flip (f: int -> int -> int) (x:int) (y:int) = f y x

let square_diff sx sy = (sx + sy) * (sx - sy)

let main (mx:int) = 
    let res = flip (twice square_diff) mx mx
    in
    assert(res = mx * (0 - mx))

let _ = main 6
let _ = main 0
let _ = main 100
let _ = main (-203)
let _ = main 1033