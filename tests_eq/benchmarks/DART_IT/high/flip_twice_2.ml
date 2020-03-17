


let twice tf tx ty = tf (tf tx ty) ty

let flip f x y = f y x

let square_diff sx sy = sx * sx - sy * sy

let main mx = 
    let res = flip (twice square_diff) mx mx
    in
    assert(res = 0 - (mx * mx))

let _ = main 6