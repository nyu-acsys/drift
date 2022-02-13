let ev d _s = 
   if _s < 0 then _s else _s * d

(* repeated positive values *)
let rec only_pos d _s0 =
   if d > 0 then begin
     let _s1 = ev d _s0 in
     only_pos (d-1) _s1
  end else _s0

let main (d:int(*-:{v:Int | true}*)) = 
  let _s00 = 1 in
  if d > 0 then begin
    let _s01 = only_pos d _s00 in
    assert (_s01 > 0)
  end