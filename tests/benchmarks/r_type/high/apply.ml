(*
USED: PLDI2011 as apply
*)

let main (mn(*-:{v:Int | true}*)) = 
    let apply f x = f x in
    let g y z = (y = z) in
    let rec k i n = if i >= n then true else (apply (g n) n && k (i+1) n) in
    assert(k 0 mn = true)  
(* in
main 20 *)