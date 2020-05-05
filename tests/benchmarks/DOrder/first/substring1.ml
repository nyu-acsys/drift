
let rec loop lk li lj = 
    if (lj < li) then
        loop lk (li) (lj+1)
    else lj < 31

let main (k:int(*-:{v:Int | true}*)) (toi:int(*-:{v:Int | true}*)) =   
    if (k >= 0 && k <= 30 && toi >= 0 && toi <= k) then
        (let i = toi in
        let j = 0 in
        assert (loop k i j = true))
    else ()