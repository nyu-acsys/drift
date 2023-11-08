let foo vv f =
  if vv = 0 then
    ev f
   else begin ev vv; foo (v-1) f end 

let main (v:int) (final:int) =
  if (final mod 2 = 0) then 
    foo v final
  else 
    foo v (if v >= 0 then final+1 else final) 
  
