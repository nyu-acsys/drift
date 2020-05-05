
let g (gx: int) = 2 * gx

let twice (tx: int) (tf: int -> int) = tf (tf tx)

let neg (nx: int) = (0 - nx)

let main_p (n:int) =
    let z = twice (g n) neg in
    if (n > 0) then assert (z >= 0)
    else assert (z <= 0)

let main (w:unit) =
	let _ = main_p 25 in
    let _ = main_p (-4) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()