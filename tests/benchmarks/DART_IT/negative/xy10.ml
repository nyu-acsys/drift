(*
Implementation error
Polka_st gets expected
*)


let rec loopa ax ay (az:int) = 
    if (ay < 20) then
        loopa (ax+10) (ay+1) az
    else 
        assert (ax <= az && ay >= az + 1)

let main (z:int(*-:{v:Int | true}*)) =
    let x = 0 in
    let y = 0 in
    loopa x y z

let _ = main 10
let _ = main 202
let _ = main 0
let _ = main (-3)
let _ = main (-100)