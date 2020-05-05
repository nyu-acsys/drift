

let f x y = assert (x <= 0 || y > 0)

let main_p (mx:int) = 
 	f mx mx

let main (w:unit) =
	let _ = main_p 10 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()