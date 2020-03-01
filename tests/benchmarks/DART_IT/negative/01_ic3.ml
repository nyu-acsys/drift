  
(*
 * Input data error
 *)

let rec loop lx ly = 
    if (lx > 20) then
        let t1 = lx in
        let t2 = ly in
        loop (t1 + t2) (t1 + t2)
    else assert (ly >= 1)
in

let main mm = 
    let x = 30 in
    let y = -1000 in
    loop x y
in
assert (main () = true) (* Should be false *)