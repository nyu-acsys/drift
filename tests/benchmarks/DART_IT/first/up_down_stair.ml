
let main (k(*-:{v:Int | true}*)) (n(*-:{v:Int | true}*)) = 

    let rec up_stair ui un = 
        if ui = un then ui
        else up_stair (ui + 1) un
    in

    let rec down_stair di dn =
        if di = 0 then di
        else up_stair (di - 1) dn 
    in

    let rec loop lk ln res = 
        if lk = 0 then ln = res
        else 
            let up =  up_stair 0 ln in
            let down = down_stair up ln in
            loop (lk - 1) ln down
    in

    if k > 0 && n > 0 then
        assert(loop k n 0 = true)
    else assert(true)
    
(* in main 4 100 *)