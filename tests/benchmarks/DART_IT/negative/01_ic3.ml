  
(*
 * Input data error
 *)


let rec loop (lx:int) (ly:int) = 
    if (lx > 20) then
        let t1 = lx in
        let t2 = ly in
        loop (t1 + t2) (t1 + t2)
    else assert (ly >= 1)
	
let main (mm:unit(*-:{v:Unit | unit}*)) = 
    let x = 30 in
    let y = -1000 in
    loop x y (* Should assert failure *)