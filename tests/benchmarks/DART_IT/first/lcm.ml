
(*
lcm x y = (x * y) / gcd x y
*)

let rec gcd (y1:int) (y2:int) =
    if (y1 <= 0 || y2 <= 0) then 0
    else if (y1 = y2) then y1
    else if (y1 > y2) then
        gcd (y1 - y2) y2
    else gcd y1 (y2 - y1)

let lcm (l1:int) (l2:int) = 
    let res = (l1 / gcd l1 l2) * l2 in
    assert(res >= l1 && res >= l2)

let main (m:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
    if m > 0 && n > 0 then
        lcm m n
    else assert(true)

let _ = main 55 13
let _ = main 10 20
let _ = main 0 0
let _ = main (-3) 4
let _ = main 59 (-19)
let _ = main 102 102
