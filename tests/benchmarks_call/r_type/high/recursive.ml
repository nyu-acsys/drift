
let rec f (g: int -> int) x = if x >= 0 then g x else f (f g) (g x)
let succ sx = sx + 1

let main_p (n:int) =
	assert (f succ n > n) 

let main (w:unit) =
	let _ = main_p 1293 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()