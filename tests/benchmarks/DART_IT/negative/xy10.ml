(*
Implementation error
*)

let main z =
    let rec loopa ax ay az = 
        if (ay < 20) then
            loopa (ax+10) (ay+1) az
        else 
            ax < az && ay < az + 1
    in

    let x = 0 in
    let y = 0 in
    loopa x y z
in assert (main 10 = true)