

let succ sx = sx + 1

let rec repeat (rf: int -> int) rn =
  if rn = 0
  then 0
  else rf (repeat rf (rn - 1))

let main_p (n:int) =
    assert (repeat succ n = n)

let main (w:unit) =
	let _ = main_p 15 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()