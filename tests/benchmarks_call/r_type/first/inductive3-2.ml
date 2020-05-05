

let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 1 then
    2 * x - 1
  else
    x

let main_p (n:int) =   
    if n >= -1 then assert(f n >= -3)
    else ()

let main (w:unit) =
	let _ = main_p 12 in
  let _ = main_p (-1) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()