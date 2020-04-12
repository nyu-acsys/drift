
let main (mm(*-:{v:Int | true}*)) =
    let rec loopa ax ay = 
        if ay < 4 then 
            loopa (ax + 4) (ay + 1)
        else ax
    in

    let rec loopb bx by = 
        if bx > 0 then
            loopb (bx - 4) (by - 1)
        else by > (-1)
    in

    let x = 0 in
    let y = 0 in

    let rx = loopa x y in
    loopb rx 4
(* in
assert (main () = true) *)