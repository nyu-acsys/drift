(*
Assertion error
*)

let rec ack m n =
  if m = 0 then n + 1
  else if n = 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

let main_p (mm:int) (mn:int) =
    if (mm >= 0 && mn >= 0)
    then assert(ack mm mn < mn)
    else ()

let main (w:unit) =
	let _ = main_p 2 3 in
    let _ = main_p 4 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()