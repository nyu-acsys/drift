
let rec loop lx ly ln = 
    if (ln < 10) then
        loop (lx + 2) (ly + 2) (ln + 1)
    else lx <> 4 || ly <> 0

let main (x:int(*-:{v:Int | true}*)) (y:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
    if (0 <= x && x <= 2 && 0 <= y && y <= 2 && 0 <= n) then
        assert(loop x y n = true)
    else ()