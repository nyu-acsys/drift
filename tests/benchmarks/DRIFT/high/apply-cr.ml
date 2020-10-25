(*
USED: PLDI2011 as apply
*)


let apply f x = f x
let g (y:int) (z:int) = assert(y = z)
let rec k i n = if i >= n then () else (apply (g n) n; k (i+1) n)

let main (n:int(*-:{v:Int | true}*)) = 
    k 0 n