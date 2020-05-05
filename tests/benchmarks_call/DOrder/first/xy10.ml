

let rec loopa ax ay az = 
    if (ay < 20) then
        loopa (ax+10) (ay+1) az
    else 
        ax > az || ay < az + 1

let main_p (z:int) =
    let x = 0 in
    let y = 0 in
    assert (loopa x y z = true)

let main (w:unit) =
	let _ = main_p 5 in
    let _ = main_p (-4) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()