(*
USED: PLDI2011 as l-zipmap
USED: PEPM2013 as l-zipmap
*)

let rec zip x y =
  if x = 0 then
    if y = 0 then
      x
    else
      (assert(false); y)
  else
    if y = 0 then
      (assert(false); x)
    else
      1 + zip (x - 1) (y - 1)

let rec map mx =
  if mx <= 0 then mx else 1 + map (mx - 1)

let main_p (n:int) =
  assert (map (zip n n) = n)

let main (w:unit) =
	let _ = main_p 31 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()