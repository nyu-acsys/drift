

let rec loop (lx:unit) = loop lx

let rec zip x y =
  if x = 0
  then
    if y = 0
    then 0
    else loop ()
  else
    if y = 0
    then loop ()
    else 1 + zip (x - 1) (y - 1)

let main_p (n:int) =
  assert (zip n n = n)

let main (w:unit) =
	let _ = main_p 456 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()