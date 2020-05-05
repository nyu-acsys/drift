

let succ (sb:int) (sf:int->unit) sx = sf (sx + 1)

let rec app (b:int) (f:int->unit) x (k:int) = 
	if k > 0 then app (b - 1) (succ (b - 1) f) (x - 1) (k - 1) else f x
	
let check (cx:int) (cy:int) = assert (cx <= cy)

let main_p (n:int) (m:int) = app n (check n) n m

let main (w:unit) =
    let _ = main_p 5 4 in
    let _ = main_p 30 (-2) in
    let _ = main_p 0 3 in
    let _ = main_p (-4) (-6) in
    let _ = main_p (-102) 1 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()