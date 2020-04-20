
let succ (sb:int) (sf:int->unit) sx = sf (sx + 1)

let rec app3 (b:int) (f:int->unit) (a:int) (g:int->(int->unit)->unit) (k:int) = 
	if k > 0 then app3 b (succ b f) b g (k - 1) else g b f

let app ax (ab:int) (af:int->unit) = af ax

let check (cx:int) (cy:int) = assert (cx <= cy)

let main (i:int(*-:{v:Int | true}*)) (j:int(*-:{v:Int | true}*)) = app3 i (check i) i (app i) j

let _ = main 2 4