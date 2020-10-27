

let rec loop (x:int) = loop x

let rec zip (x:int) (y:int) =
 if x = 0 then
   if y = 0 then x
   else assert false
 else if y = 0 then assert false
 else 1 + zip (x-1) (y-1)

let rec map (x:int) =
  if x = 0 then x else 1 + map (x - 1)

let main (n:int(*-:{v:Int | true}*)) =
	assert(map (zip n n) = n)