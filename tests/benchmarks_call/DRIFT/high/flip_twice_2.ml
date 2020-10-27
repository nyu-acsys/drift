
let twice tf (tx:int) (ty:int) = tf (tf tx ty) ty

let flip (f: int -> int -> int) (x:int) (y:int) = f y x

let square_diff sx sy = (sx + sy) * (sx - sy)

let main_p (mx:int) = 
    let res = flip (twice square_diff) mx mx
    in
    assert(res = mx * (0 - mx))

let main (w:unit) =
	let _ = main_p 6 in
    let _ = main_p 0 in
    let _ = main_p (-34) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()