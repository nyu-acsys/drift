
let main (k(*-:{v:Int | v > 0}*)) (n(*-:{v:Int | v > 0}*)) = 

    let rec up_stair ui un = 
        if ui = un then ui
        else up_stair (ui + 1) un
    in

    let rec down_stair di dn =
        if di = 0 then di
        else down_stair (di - 1) dn 
    in

    let rec loop lk ln res = 
        if lk = 0 then ln = res
        else 
            let up = up_stair 0 ln in
            let down = down_stair up ln in
            loop (lk - 1) ln (down + ln)
    in

    assert(loop k n n = true)
    
(* let _ = main 4 100
let _ = main 10 20
let _ = main 0 0
let _ = main (-1) (-20) *)