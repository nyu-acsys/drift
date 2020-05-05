(*
USED: PLDI2011 as a-init
*)

let make_array n i = assert (0 <= i && i < n); 0
let update i a x j :int= if j > i - 1 && j <= i then x else a (j)
let rec init i n a =
  if i < 0 then init 0 n a
  else if i >= n then a else init (i + 1) n (update i a 1)
let main_p k n i =
  if i >= k && i >= 0 && i < n then
    let x = init k n (make_array n) in
    if 0 <= i && i < n then
    assert (x i >= 1)
  else ()
let main (w:unit) =
	let _ = main_p 2 5 3 in
    let _ = main_p (-10) 10 2 in
	()

let _ = main ()