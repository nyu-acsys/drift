

let f g x =
  if x > 0 then
    g x
  else
    1

let decr x = x - 1
	
let main (n:unit) =
	if n >= 3 then assert(f decr n > 0)
	else assert(true)
let _ = main ()