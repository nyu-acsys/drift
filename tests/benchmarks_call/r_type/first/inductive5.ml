
let rec f x =
  if x < -2 then
    f (-3)
  else if x < 2 then
    2 * x - 1
  else if x <= 2 then
    f (2 * x - 1)
  else
    x

let main_p (n:int) =
	if n > -2 then assert(f n >= (-3))
	else ()

let main (w:unit) =
	let _ = main_p 3 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()