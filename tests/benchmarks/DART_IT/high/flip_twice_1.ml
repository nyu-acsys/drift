

let twice tf (tx:int) (ty:int) = tf (tf tx ty) ty

let flip (f: int -> int -> int) (x:int) (y:int) = f y x

let square_diff sx sy = (sx + sy) * (sx - sy)

let main (mx:int(*-:{v:Int | true}*)) (my:int(*-:{v:Int | true}*)) =
    if mx >= 0 && my >= 0 && mx > my then
        assert(flip (twice square_diff) my mx >= my)
    else if mx >= 0 && my >= 0 && mx < my then assert(flip (twice square_diff) mx my >= mx)
    else assert(true)

(* Proof, for a > b >= 0
a^2 - b^2 = (a + b)(a - b) >= (a + b) > b
(a^2 - b^2)^2 - b^2 = (a^2 - b^2 + b)(a^2 - b^2 - b) > (a^2 - b^2 + b) > 2b > b 
*)