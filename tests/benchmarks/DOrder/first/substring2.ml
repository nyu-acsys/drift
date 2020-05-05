

let rec loop lk li lj = 
    if (li < lk) then
        loop lk (li+1) (lj+1)
    else lj < 31

let main (k:int(*-:{v:Int | true}*)) (from:int(*-:{v:Int | true}*)) =   
    if (k >= 0 && k <= 30 && from >= 0 && from <= k) then
        (let i = from in
        let j = 0 in
        assert (loop k i j = true))
    else ()