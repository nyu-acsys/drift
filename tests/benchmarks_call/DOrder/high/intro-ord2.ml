
let rec app (a:int) (f:int->unit) (x:int) (k:int) = 
	if k > 0 then 
		app a f (x + 1) (k - 1)
	else f x

let check (cx:int) (cy:int) = assert (cx <= cy)

let main_p (i:int) (j:int) = app i (check i) i j

let main (w:unit) =
    let _ = main_p 5 2 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()