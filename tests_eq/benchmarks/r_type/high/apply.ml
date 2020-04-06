(*
USED: PLDI2011 as apply
*)


let apply (f: int -> bool) x = f x
let g (y:int) (z:int) = (y = z)
let rec k i n = if i >= n then true else (apply (g n) n && k (i+1) n)

let main (mn:int) = 
    assert(k 0 mn = true) 

let _ = main 20
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)