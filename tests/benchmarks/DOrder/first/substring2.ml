

let rec loop lk li lj = 
    if (li < lk) then
        loop lk (li+1) (lj+1)
    else lj < 31

let main (k:int(*-:{v:Int | true}*)) (from:int(*-:{v:Int | true}*)) =   
    if (k >= 0 && k <= 30 && from >= 0 && from <= k) then
        (let i = from in
        let j = 0 in
        assert (loop k i j = true))
    else assert(true)

let _ = main 20 5
let _ = main 9 0
let _ = main 0 9
let _ = main 5 20
let _ = main 60 60
let _ = main (-1) (-1)