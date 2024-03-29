let add_stock n = ev 2; ev (-n); ()

let add_money n = ev 1; ev (-n); ()

let sell n = ev 2; ev n; ()

let buy n = ev 1; ev n; ()

let activity f n = f n; ev 0; ()

let rec repeat n = 
  if (n<=0) then 0 
  else 
    begin
      activity buy 1; 
      activity sell 1; 
      1 + repeat (n-1)
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
