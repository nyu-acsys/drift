let make_array l i = assert(0<=i && i<l); 0
let update i l des x = let _ = des i in ()
let rec inc3 m src i =
 if i>=m
 then ()
 else
   begin
     update i m src ((src i)+1);
     inc3 m src (i+1)
   end

let rec increcursive m src i h = 
  if h = 0 then inc3 m src i
  else increcursive m src i (h-1)

let main (n(*-:{v:Int|true}*)) =
 if n>0 then inc3 n (make_array n) 0 else ()
