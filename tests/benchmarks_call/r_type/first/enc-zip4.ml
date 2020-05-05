

let rec zip x y =
  if x = 0
  then if y = 0 then 0 else (assert(false); y)
  else if y = 0 then (assert(false); x)
  else 1 + zip (x - 1) (y - 1)

let main_p (n:int) =
    let m = zip n n in
    assert (m = n)

let main (w:unit) =
	let _ = main_p 10 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()