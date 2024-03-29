

let rec loop (x:unit) = loop x
let rec zip (x:int) (y:int) =
 if x = 0 then
   if y = 0 then 0
     else loop () (* Dead END! *)
 else if y = 0 then loop () (* Dead END! *)
   else 1 + zip (x - 1) (y - 1)

let rec map (mx:int) =
  if mx = 0 then 0 else 1 + map (mx - 1)

let main (n:int(*-:{v:Int | true}*)) =
  assert(map (zip n n) >= n)