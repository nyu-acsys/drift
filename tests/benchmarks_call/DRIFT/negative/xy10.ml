(*
Implementation error
Polka_st gets expected
*)


let rec loopa ax ay (az:int) = 
    if (ay < 20) then
        loopa (ax+10) (ay+1) az
    else 
        assert (ax <= az && ay >= az + 1)

let main_p (z:int) =
    let x = 0 in
    let y = 0 in
    loopa x y z

let main (w:unit) =
	let _ = main_p 10 in
    let _ = main_p 0 in
    let _ = main_p (-100) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()