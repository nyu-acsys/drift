
let rec loop lx ly ln = 
    if (ln < 10) then
        loop (lx + 2) (ly + 2) (ln + 1)
    else lx <> 4 || ly <> 0

let main x y n = 
    let ans = 
        if (0 <= x && x <= 2 && 0 <= y && y <= 2 && 0 <= n) then
            loop x y n
        else true 
    in assert(ans = true)

let _ = main 0 0 4       
let _ = main 0 1 3
let _ = main 1 0 0
let _ = main 2 0 3
let _ = main 4 0 5
let _ = main 0 4 1
let _ = main (-1) (-1) (-1)