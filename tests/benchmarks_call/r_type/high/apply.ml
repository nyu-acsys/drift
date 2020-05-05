(*
USED: PLDI2011 as apply
*)


let apply (f: int -> bool) x = f x
let g (y:int) (z:int) = (y = z)
let rec k i n = if i >= n then true else (apply (g n) n && k (i+1) n)

let main_p (mn:int) = 
    assert(k 0 mn = true) 

let main (w:unit) =
	let _ = main_p 20 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()