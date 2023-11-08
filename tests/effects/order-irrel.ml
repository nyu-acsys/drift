(* 
there are two particular events (let us treat them as integers
 c and -c for some c) such that at most one of them is permitted 
 to occur during any execution
*)
let rec order d c = 
  if (d > 0) then begin
     begin if ( d % 2 == 0 ) then ev c else ev(-c) end;
     order (d - 2) c
  end else 0

let main (dd:int) (cc:int) = 
  order dd cc

