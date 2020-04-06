


let rec loop (ly:int) (lz:int) (lc:int) = 
    if lc < 36 then
        if lz >= 0 && lz < 4608 then
            loop ly (lz+1) (lc+1)
        else -1
    else lz

let main (y:int) = 
    let c = 0 in
    let z = y * 36 in
    let ans = loop y z c in
    if y < 0 then assert(ans >= 0 && ans <= 4608)
	else ()

let _ = main 128
let _ = main (-234)
let _ = main 0
let _ = main (-3)