  
(*
 * Input data error
 *)



let rec loop lx ly = 
    if (lx > 20) then
        let t1 = lx in
        let t2 = ly in
        loop (t1 + t2) (t1 + t2)
    else ly >= 1
	
let main mm = 
    let x = 30 in
    let y = -1000 in
    assert (loop x y = true) (* Should be false *)

let _ = main ()