
let g (gx: int) = 2 * gx

let twice (tx: int) (tf: int -> int) = tf (tf tx)

let neg (nx: int) = (0 - nx)

let main (n:int(*-:{v:Int | true}*)) =
    let z = twice (g n) neg in
    if (n > 0) then assert (z >= 0)
    else assert (z <= 0)

let _ = main 1
let _ = main (-1)