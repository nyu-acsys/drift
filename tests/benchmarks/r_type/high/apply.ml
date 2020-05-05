(*
USED: PLDI2011 as apply
*)


let apply (f: int -> bool) x = f x
let g (y:int) (z:int) = (y = z)
let rec k i n = if i >= n then true else (apply (g n) n && k (i+1) n)

let main (mn:int(*-:{v:Int | true}*)) = 
    assert(k 0 mn = true)