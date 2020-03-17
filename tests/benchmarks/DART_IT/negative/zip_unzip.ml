
let main n = 
	let helper hg hx hy = hg (hx + 1) (hy + 1)
	in

	let rec unzip ux uk =
	  if ux = 0 then
	    uk 0 0
	  else
	    unzip (ux - 1) (helper uk)
	in

	let rec zip zx zy =
	  if zx = 0 then (assert (zy = 0); 0)
	  else (assert (zy <> 0); 1 + zip (zx - 1) (zy - 1))
	in

	assert(unzip n zip = n)
in main (-10)