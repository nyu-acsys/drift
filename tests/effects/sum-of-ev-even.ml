let rec sum x = 
  ev x;
  if x <= 0 then 0 else sum (x - 2)

let main (v:int) =
  if v % 2 = 0 then sum v else sum (v + 1)
