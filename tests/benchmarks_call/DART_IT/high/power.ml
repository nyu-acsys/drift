

let comp (cf:int -> int) (cg:int -> int) cx = cf (cg cx)

let id (dx:int) = dx

let rec power (f: int -> int) i = 
    if i = 0 then id
    else comp f (power f (i - 1))

let succ (sx:int) = sx + 1

let main_p (m:int) (n:int) = 
	if m > 0 then
    	assert(power succ m n >= n)
	else ()

let main (w:unit) =
	let _ = main_p 4 2 in
    let _ = main_p 0 0 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()