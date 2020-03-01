
let rec gcd y1 y2 =
    if (y1 <= 0 || y2 <= 0) then 0
    else if (y1 = y2) then y1
    else if (y1 > y2) then
        gcd (y1 - y2) y2
    else gcd y1 (y2 - y1)
in

let lcm l1 l2 = 
    if l1 > 0 && l2 > 0 then
        let res = (l1 * l2) / gcd l1 l2 in
        res >= l1 && res >= l2
    else false
in lcm 55 13