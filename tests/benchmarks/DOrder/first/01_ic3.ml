  
(*
 * IC3 motivating example
 *)


let rec loop lx ly = 
    if (ly < 45) then
        let t1 = lx in
        let t2 = ly in
        loop (t1 + t2) (t1 + t2)
    else assert (ly >= 1)

let main (mm:unit(*-:{v:Unit | unit}*)) = 
    let x = 1 in
    let y = 1 in
    loop x y

let _ = main ()