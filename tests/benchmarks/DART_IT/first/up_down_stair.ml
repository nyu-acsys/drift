
let rec up_stair i n = 
    if i = n then i
    else up_stair (i + 1) n
in

let rec down_stair i n =
    if i = 0 then i
    else up_stair (i - 1) n 
in

let rec loop k n res = 
    if k = 0 then n = res
    else 
        let up =  up_stair 0 n in
        let down = down_stair up n in
        loop (k - 1) n down
in

let main k n = 
    if k > 0 && n > 0 then
        loop k n 0
    else false
in main 4 100