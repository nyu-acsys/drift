

let g gx = 2 * gx

let twice tx tf = tf (tf tx)

let neg nx = (0 - nx)

let main (n:int) =
    let z = twice (g n) neg in
    if (n > 0) then assert (z >= 0)
    else assert (z <= 0)

let _ = main 3
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)