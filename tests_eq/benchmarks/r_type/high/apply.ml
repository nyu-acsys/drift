(*
USED: PLDI2011 as apply
*)


let apply f x = f x
let g y z = assert (y = z)
let rec k i n = if i >= n then true else (apply (g n) n && k (i+1) n)

let main mn = 
    k 0 mn 

let _ = main 20