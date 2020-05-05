(*
USED: PEPM2013 as sum_intro
*)

let add x y = x + y

let rec sum n =
  if n <= 0 then
    0
  else
    add n (sum (n - 1))

let main_p (mn:int) =
	assert (mn <= sum mn)

let main (w:unit) =
	let _ = main_p 123 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()