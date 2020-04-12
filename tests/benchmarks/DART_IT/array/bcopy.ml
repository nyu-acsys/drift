
let main (mn(*-:{v:Int | true}*)) (mm(*-:{v:Int | true}*)) =

	let rec bcopy_aux src des i m =
	  if i >= m then true
	  else
	    (
	      set des i (get src i);
	      0 <= i && i < (len src) && bcopy_aux src des (i+1) m
	    )
	in

	let bcopy src des = bcopy_aux src des 0 (len src) in

  	let array1 = make mn 0 in
  	let array2 = make mm 0 in
  	if mn <= mm then bcopy array1 array2 else true
