

let rec loopa ax ay az = 
    if (ay < 20) then
        loopa (ax+10) (ay+1) az
    else 
        ax > az || ay < az + 1

let main (z:int(*-:{v:Int | true}*)) =
    let x = 0 in
    let y = 0 in
    assert (loopa x y z = true)

let _ = main 5
let _ = main (-4)