let ev d _s = 
  if d > 0 then (true, (snd _s))
  else (fst _s, true)

(* infer that return value (e,o) is not both true *)
let rec order d c _s0 =
  if(d>0) then begin
    let _s1 = (
        if ( d % 2 == 0 ) then 
          ev c _s0
        else
          ev (-c) _s0) in
    order (d - 2) (c * d) _s1
  end else _s0

let main d:Nat c:Nat = 
  order d c (false,false)

(* assume c!=0. Property both c and -c cannot happen *)