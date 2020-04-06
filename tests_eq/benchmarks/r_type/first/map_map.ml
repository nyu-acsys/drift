

let rec map x =
 if x = 0 then x else 1 + map (x - 1)

let main (n:int) =
    if n >= 0 then assert(map (map n) = n)
    else assert(true)

let _ = main 12329
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)