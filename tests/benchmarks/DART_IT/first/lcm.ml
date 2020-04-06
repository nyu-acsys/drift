
(*
Imprecision:
*)

let main (m(*-:{v:Int | v > 0}*)) (n(*-:{v:Int | v > 0}*)) =
    let rec gcd y1 y2 =
        if (y1 <= 0 || y2 <= 0) then 0
        else if (y1 = y2) then y1
        else if (y1 > y2) then
            gcd (y1 - y2) y2
        else gcd y1 (y2 - y1)
    in

    let lcm l1 l2 = 
        let res = (l1 / gcd l1 l2) * l2 in
        assert(res >= l1 && res >= l2)
    in
 
    lcm m n

(* let _ = main 55 13
let _ = main 10 20
let _ = main 0 0
let _ = main (-3) 4
let _ = main 59 (-19)
let _ = main 102 102 *)