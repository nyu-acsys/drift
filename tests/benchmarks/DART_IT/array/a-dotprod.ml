(*
USED: PLDI2011 as a-prod
*)

let main (z(*-:{v:Int | v = 0}*)) (n(*-:{v:Int | v > 0}*)) =
    let rec dotprod dn vd1 vd2 di sum =
      if di >= dn then
        sum
      else
        dotprod dn vd1 vd2 (di + 1) (sum + (get vd1 di) * (get vd2 di))
    in

    let v1 = make n 0 in
    let v2 = make n 1 in
    dotprod n v1 v2 z z; ()
