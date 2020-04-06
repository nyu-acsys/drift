(*
Assertion error
*)



let rec loopa ax ay az = 
    if (ax < 10) then 
        loopa (ax + 1) (ay + 1) (az - 2)
    else az

let rec loopb bx by bz = 
    if (bx >= 1) then 
        (let rz = bz + 2 in
        let rx = bx - 1 in
        let ry = by - 1 in
        loopb rx ry rz)
    else bz < 1

let main (mm:unit) =
    let x = 0 in
    let y = 0 in
    let z = 0 in

    let rsz = loopa x y z in
    let rsx = 10 in
    assert (loopb rsx rsx rsz = false)

let _ = main ()