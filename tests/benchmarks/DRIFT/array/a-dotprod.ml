(*
USED: PLDI2011 as a-prod
*)

let rec dotprod dn (vd1: int array) (vd2: int array) di sum =
  if di >= dn then
    sum
  else
    dotprod dn vd1 vd2 (di + 1) (sum + (Array.get vd1 di) * (Array.get vd2 di))

let main (z(*-:{v:Int | true}*)) (n(*-:{v:Int | true}*)) =
    if z = 0 && n > 0 then
	    let v1 = Array.make n 1 in
	    let v2 = Array.make n 1 in
	    dotprod n v1 v2 z z; ()
	else ()
