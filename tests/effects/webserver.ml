let rec listener npool pend n = 
  if n <= 0 then 0
  else
    if pend < npool then 
      begin
        ev 0;
        listener npool (pend+1) (n-1)
      end
    else if pend > 0 then
      begin
        ev 1;
        listener npool (pend-1) (n-1)
      end
    else
      begin
        ev 2;
        listener npool pend (n-1)
      end

let main (npool:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if npool >= 0 then
    listener npool 0 n
  else 0