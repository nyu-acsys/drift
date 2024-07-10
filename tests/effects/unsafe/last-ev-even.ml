let rec foo vv f =
  if vv = 0 then begin ev f; 0 end
  else begin ev vv; foo (vv-1) f end 

let main (v:int(*-:{v:Int | true}*)) (final:int(*-:{v:Int | true}*)) =
  if (final mod 2 = 0) then 
    foo v final
  else 
    foo v (if v >= 0 then final+1 else final) 
  
