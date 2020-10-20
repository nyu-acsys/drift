
let twice tf (tx:int) (ty:int) = tf (tf tx ty) ty

let flip (f: int -> int -> int) (x:int) (y:int) = f y x

let square_diff sx sy = (sx + sy) * (sx - sy)

let main_p (mx:int) (my:int) =
    if mx >= 0 && my >= 0 && mx > my then
        assert(flip (twice square_diff) my mx >= my)
    else if mx >= 0 && my >= 0 && mx < my then assert(flip (twice square_diff) mx my >= mx)
    else ()

let main (w:unit) =
	let _ = main_p 16 32 in
    let _ = main_p 30 2 in
    (* let _ = 
        for i = 1 to 1000000 do
        main_p (Random.int 1000) (Random.int 1000)
        done in *)
	()

let _ = main ()

(* Proof, for a > b >= 0
a^2 - b^2 = (a + b)(a - b) >= (a + b) > b
(a^2 - b^2)^2 - b^2 = (a^2 - b^2 + b)(a^2 - b^2 - b) > (a^2 - b^2 + b) > 2b > b 
*)