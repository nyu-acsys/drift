

let rec map x =
 if x = 0 then x else 1 + map (x - 1)

let main_p (n:int) =
    if n >= 0 then assert(map (map n) = n)
    else ()

let main (w:unit) =
	let _ = main_p 15 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()