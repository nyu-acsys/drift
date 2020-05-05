(*
 * Implementation error
 *)

let rec repeat (f: int -> int) n s =
  if n = 0 then
    s
  else
    repeat f (n - 1) s

let succ (x:int) = x + 1

let main_p (n:int) =
	if n > 0 then assert(repeat succ n 0 >= n)
	else ()

let main (w:unit) =
	let _ = main_p 0 in
    let _ = main_p 10 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()