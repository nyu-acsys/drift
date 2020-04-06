

let rec loop lk li lj = 
    if (li < lk) then
        loop lk (li+1) (lj+1)
    else lj < 31

let main (k:int) (from:int) =   
    if (k >= 0 && k <= 30 && from >= 0 && from <= k) then
        (let i = from in
        let j = 0 in
        assert (loop k i j = true))
    else assert(true)

let _ = main 5 3
let _ = main 30 3
let _ = main 20 33
let _ = main 0 0
let _ = main 100 94
let _ = main (-3) (-55)