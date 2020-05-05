

let rec up_stair (ui:int) (un:int) = 
    if ui = un then ui
    else up_stair (ui + 1) un


let rec down_stair (di:int) (dn:int) =
    if di = 0 then di
    else down_stair (di - 1) dn 


let rec loop (lk:int) (ln:int) (res:int) = 
    if lk = 0 then ln = res
    else 
        (let up = up_stair 0 ln in (* up = ln*)
        let down = down_stair up ln in (* down = 0*)
        loop (lk - 1) ln (down + ln))

let main_p (k:int) (n:int) = 
    if k > 0 && n > 0 then
        assert(loop k n n = true)
    else ()

let main (w:unit) =
	let _ = main_p 4 100 in
    let _ = main_p 10 20 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()