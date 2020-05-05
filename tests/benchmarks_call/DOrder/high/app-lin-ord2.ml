
let app (ab:int) (af:int->unit) ax = af ax 

let check (cx:int) (cy:int) = assert (cx = cy) 

let main_p (a:int) (b:int) =
    app (4 * a + 2 * b) (check (4 * a + 2 * b)) (4 * a + 2 * b) 

let main (w:unit) =
	let _ = main_p 1 1 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()