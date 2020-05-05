
let f g x =
  if x > 0 then
    g x
  else
    1

let decr dx = dx - 1
	
let main (n:int(*-:{v:Int | true}*)) = 
	if n >= 3 then assert(f decr n > 0)
	else ()