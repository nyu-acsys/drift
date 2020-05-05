(*
USED: PLDI2011 as mult
USED: PEPM2013 as mult
*)

let rec mult n m =
  if n <= 0 || m <= 0 then
    0
  else
    n + mult n (m - 1)

let main_p (mn:int) =
    assert (mn <= mult mn mn)

let main (w:unit) =
	let _ = main_p 25 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
