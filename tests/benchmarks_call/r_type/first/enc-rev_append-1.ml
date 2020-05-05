
let rec append x y =
  if x = 0 then
    y
  else
    1 + append (x - 1) y

let rec rev n =
  if n = 0
  then 0
  else append (rev (n - 1)) 1

let main_p (mn:int) (mm:int) =
  if mn >= 0 then assert (rev mn = mn)
	else ()

let main (w:unit) =
	let _ = main_p 3 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()