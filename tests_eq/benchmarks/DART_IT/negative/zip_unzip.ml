

let helper hg hx hy = hg (hx + 1) (hy + 1)

let rec unzip ux uk =
  if ux = 0 then
    uk 0 0
  else
    unzip (ux - 1) (helper uk)


let rec zip zx zy =
  if zx = 0 then (assert (zy = 0); 0)
  else (assert (zy <> 0); 1 + zip (zx - 1) (zy - 1))


let main (n:int) = 
	if n < 0 then
		assert(unzip n zip = n)
	else ()

let _ = main (-10)
let _ = main (-20)
let _ = main 0
let _ = main 100