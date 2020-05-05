
let f g x =
  if x > 0 then
    g x
  else
    1

let decr dx = dx - 1
	
let main_p (n:int) = 
	if n >= 3 then assert(f decr n > 0)
	else ()

let main (w:unit) =
	let _ = main_p 79 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()