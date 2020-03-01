

let twice tf tx ty = tf (tf tx ty) ty
in

let flip f x y = f y x
in

let square_diff sx sy = sx * sx - sy * sy
in

let main mx = 
    let res = flip (twice square_diff) mx mx
    in
    assert(res = 0 - (mx * mx))
in main 6