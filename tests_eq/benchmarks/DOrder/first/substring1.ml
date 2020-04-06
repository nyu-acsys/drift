
let rec loop lk li lj = 
    if (lj < li) then
        loop lk (li) (lj+1)
    else lj < 31

let main (k:int) (from:int) =   
    if (k >= 0 && k <= 100 && from >= 0 && from <= k) then
        (let i = from in
        let j = 0 in
        assert (loop k i j = true))
    else assert(true)

let _ = main 5 3
let _ = main 30 3
let _ = main 20 33
let _ = main 0 0
let _ = main 500 78
let _ = main (-3) (-55)