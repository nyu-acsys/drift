

let rec loop (ly:int) (lz:int) (lc:int) = 
    if lc < 36 then
        if lz >= 0 && lz < 4608 then
            loop ly (lz+1) (lc+1)
        else -1
    else lz

let main_p (y:int) = 
	if y < 0 then
	    (let c = 0 in
	    let z = y * 36 in
	    let ans = loop y z c in
	    assert(ans >= 0 && ans <= 4608))
	else ()

let main (w:unit) =
	let _ = main_p (-234) in
    let _ = main_p (-3) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()