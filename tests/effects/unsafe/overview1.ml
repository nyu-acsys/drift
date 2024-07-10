(* 
 there are two particular events
 (let us treat them as integers c and -c for some c)
 and a temporal ordering:
 q0 --(c)--> q1 --(-c)--> q2
 where q0 and q1 are error states
*)

let rec busy n t =
  if n <= 0 then begin 
    ev t; 
    0
  end else busy (n - 1) t

let main (x:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  ev x;
  busy n x


(*
if dd > 0 then begin ev dd; ev(-dd) end
  else begin ev(-dd); ev dd; end
*)

