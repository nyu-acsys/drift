

let rec loop (lx:int) = loop lx

let rec zip (x:int) (y:int) =
 if x = 0 then
   if y = 0 then 0
     else loop y
 else if y = 0 then loop x
   else 1 + zip (x - 1) (y - 1)

let rec map (mx:int) =
  if mx = 0 then mx else 1 + map (mx - 1)

let main (n:int(*-:{v:Int | true}*)) =
	if n >= 0 then assert(map (zip n n) = n)
	else ()