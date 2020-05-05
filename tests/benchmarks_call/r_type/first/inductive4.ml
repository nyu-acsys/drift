

let rec f g x =
  if x < (-3) then (* x<=-4 *)
    f g (-4)
  else if x <= 1 then (* -3<=x<=1 *)
    g x
  else (* x>1 *)
    f (f g) (x - 2)

let incr y = y + 1

let main_p (n:int) =   
    assert(f incr n >= -3)

let main (w:unit) =
	let _ = main_p 12 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
