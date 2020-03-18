(*
 * Input data error
 *)

let rec repeat f n s =
  if n = 0 then
    s
  else
    f (repeat f (n - 1) s)

let succ x = x + 1

let main n = 
	if n < 0 then
		assert(repeat succ n 0 >= n)

let _ = main (-1) (* Overflow *)