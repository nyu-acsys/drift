

let app (ab:int) (af:int->(int->unit)->unit) aa (ag:int->unit) = af aa ag

let f (fx:int) (fa:int) (fk:int->unit) = fk fx

let check (x:int) (y:int) = assert (x = y)

let main_p (a:int) (b:int) =
    app (4 * a + 2 * b) (f (4 * a + 2 * b)) (4 * a + 2 * b) (check (4 * a + 2 * b))

let main (w:unit) =
	let _ = main_p 1 1 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()