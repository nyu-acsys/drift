
let rec repeat (f:int->int) n s =
  if n = 0 then
    s
  else
    f (repeat f (n - 1) s)

let succ x = x + 1

let main_p (mn:int) =
    assert(repeat succ mn 0 >= mn) 

let main (w:unit) =
    let _ = main_p 2 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()