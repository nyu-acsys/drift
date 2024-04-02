let add_stock n6 = ev 2; ev (-n6); ()

let add_money n5 = ev 1; ev (-n5); ()

let sell n4 = ev 2; ev n4; ()

let buy n3 = ev 1; ev n3; ()

let activity f n2 = f n2; ev 0; ()

let rec repeat n1 = 
  if (n1<=0) then 0 
  else 
    begin
      activity buy 1; 
      activity sell 1; 
      1 + repeat (n1-1)
    end

let main (budget:int(*-:{v:Int | true}*)) (stock:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if (stock >= budget && budget >= n && n>0) then
    begin
      activity add_stock stock;
      activity add_money budget;
      repeat n
    end
  else
    0
