let sell n1 = ev 2; ev n1; ()

let buy n2 = ev 1; ev n2; ()

let activity f n3 = f n3; ev 0; ()

let rec repeat n4 = 
  if (n4<=0) then 0 
  else 
    begin
      activity buy 1; 
      activity sell 1; 
      1 + repeat (n4-1)
    end

let main (budget:int(*-:{v:Int | v >= 0}*)) (stock:int(*-:{v:Int | v >= 0}*)) (n:int(*-:{v:Int | v > 0}*)) =
  if (stock >= budget && budget >= n) then
    begin
      activity sell (-stock);
      activity buy (-budget);
      repeat n
    end
  else
    0
