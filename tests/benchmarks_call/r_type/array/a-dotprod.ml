(*
USED: PLDI2011 as a-prod
*)

let make_array n i = assert (0 <= i && i < n); 0
let rec dotprod n v1 v2 i sum =
  if i >= n then
    sum
  else
    dotprod n v1 v2 (i + 1) (sum + v1 i * v2 i)
let main_p z n =
  let v1 = make_array n in
  let v2 = make_array n in
  if z=0 then (dotprod n v1 v2 z z; ()) else ()

let main (w:unit) =
	let _ = main_p 0 4 in
	let _ = main_p 10 10 in
	(* let _ = 
			for i = 1 to 1000 do
				main_p (Random.int 1000) (Random.int 1000)
			done in *)
	()

let _ = main ()