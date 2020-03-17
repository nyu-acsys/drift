  
(*
 * Input data error
 *)


let main mm = 
	let rec loop lx ly = 
	    if (lx > 20) then
	        let t1 = lx in
	        let t2 = ly in
	        loop (t1 + t2) (t1 + t2)
	    else ly >= 1
	in

    let x = 30 in
    let y = -1000 in
    assert (loop x y = true) (* Should be false *)
in
main ()