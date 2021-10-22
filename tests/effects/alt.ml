let ev d _s = 
    if _s = 2 then 2 (* loop in error state *)
    else begin
        if d > 0 && _s > 0 then 
            2 (* indicate error *)
        else (if d < 0 && _s < 0 then
            2
        else
            d) (* remember the current d for next time *)
    end

let nondet (i:int) = (i * i * i * 500) mod 199

(* alternate events 1,-1,1,-1,... *)
let rec alt n b _s0 =
   if n > 0 then begin
      let _s1 = (if b then ev (-1) _s0 else ev 1 _s0) in
      let notb = (if b then false else true) in
      alt (n-1) notb _s1 
  end else _s0

let main (n:int(*-:{v:Int | true}*)) = 
  let _s00 = 0 in
  if n > 0 then begin
    let _s01 = alt n false _s00 in
    assert (_s01 < 2)
  end