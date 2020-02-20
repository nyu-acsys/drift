

let rec map f x =
  if x = 0
  then x
  else x + (f (map f (x - 1))) in

let main n =
	assert(map (fun k -> k) n = (n + 1) * n /2)
	&& assert(map (fun k -> k + 1) n = (n + 3) * n /2)
in

let out = main 4 && main 12 
in
out