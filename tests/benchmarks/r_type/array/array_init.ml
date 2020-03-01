(*
	False positive
*)

let rec init idx n a =
  if idx>=n then a
  else (set a idx 1; init (idx+1) n a)
in

let main n i =
	let x = init 0 3 (make 3 0) in
	if 0<=i && i<n then
    	assert (get x i >=1) (* check that the array has been initialized *)
    else false
in main 3 2