
let rec fib n =
  if n < 2 then 1 else
    fib (n - 1) + fib (n - 2)

let main_p (mn:int) =
    assert (fib mn >= mn)

let main (w:unit) =
	let _ = main_p 34 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
