(*
USED: PLDI2011 as max
USED: PEPM2013 as max
*)


let max (max2: int -> int -> int) (mx:int) (my:int) (mz:int) = max2 (max2 mx my) mz
let f (fx:int) (fy:int) = if fx >= fy then fx else fy

let main_p (x:int) (y:int) (z:int) =
    let m = max f x y z in
    assert (f x m = m)

let main (w:unit) =
	let _ = main_p 3 4 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()