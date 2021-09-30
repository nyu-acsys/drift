let rec order d c =
  if(d>0) then begin
    if ( d % 2 == 0 ) then 
      ev c
    else
      ev (-c);
    order (d - 2) (c * d)
  end    

let main d:Nat c:Nat = order d c

(* Property both c and -c cannot happen *)