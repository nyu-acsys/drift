(*
USED: PLDI2011 as a-init
*)

let rec init idx n a =
  if idx >= n then a else (set a idx 1; init (idx + 1) n a) 
in
  init 0 3 (make 3 0)
(*
let main k n i =
  if k >= 0 then
    let x = init k n (make n 0) in
      if 0 <= i && i < n then
        assert (get x i >= 1)
      else false
  else false
in
main 0 3 2
*)
