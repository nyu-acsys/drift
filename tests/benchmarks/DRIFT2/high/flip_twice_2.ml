


let twice tf (tx:int) (ty:int) = tf (tf tx ty) ty

let flip (f: int -> int -> int) (x:int) (y:int) = f y x

let square_diff sx sy = (sx + sy) * (sx - sy)

let main (mx:int(*-:{v:Int | true}*)) = 
    let res = flip (twice square_diff) mx mx
    in
    assert(res = mx * (0 - mx))

(*
mx : { v: int | v = mx}
(0 - mx) : {v: int | v = -mx}
mx * (0 - mx): {v:int | top}
*)