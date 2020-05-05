

let rec loop x i =
  if i < 0 then
    x
  else if x < 1 then
    loop (x - 1) (i - 1)
  else if x > 2 then
    loop x (i - 1)
  else
    loop (3 - x) (i - 1)

let main_p (n:int) =    
    assert (loop 1 n >= 0)

let main (w:unit) =
	let _ = main_p 4 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()