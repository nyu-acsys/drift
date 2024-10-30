
(* let choose x = 
  if nondet then begin
    ev 1; 
    0
  end else if nondet then begin
    ev 2; 
    0
  end else begin
    ev 3;
    0
  end *)

let rec sum n =
  if n = 0 then 0
  else begin
    if nondet then
      ev 1
    else if nondet then
      ev 2
    else
      ev 3;
    sum (n-1) end

let main (n:int(*-:{v:Int | v>0}*)) = 
  sum n
