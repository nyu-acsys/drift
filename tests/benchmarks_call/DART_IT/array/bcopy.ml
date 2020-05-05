
let rec bcopy_aux (src: int array) (des: int array) i m =
  if i >= m then true
  else
    (
      Array.set des i (Array.get src i);
      0 <= i && i < (Array.length src) && bcopy_aux src des (i+1) m
    )

let bcopy (src: int array) (des: int array) = bcopy_aux src des 0 (Array.length src)

let main_p (mn:int) (mm:int) =
		if mn > 0 && mn <= mm then
	  	let array1 = Array.make mn 0 in
	  	let array2 = Array.make mm 0 in
	  	assert(bcopy array1 array2 = true) 
		else ()

let main (w:unit) =
	let _ = main_p 5 10 in
	let _ = main_p 12 26 in
	()

let _ = main ()