(*
 * Implementation error
 *)

let rec repeat (f: int -> int) n s =
  if n = 0 then
    s
  else
    repeat f (n - 1) s

let succ (x:int) = x + 1

let main (n:int(*-:{v:Int | true}*)) =
	if n > 0 then assert(repeat succ n 0 >= n)
	else ()