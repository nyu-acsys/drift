(*
USED: PEPM2013 as sum_intro
*)

let add x y = x + y

let rec sum m =
  if m <= 0 then
    0
  else
    add m (sum (m - 1))

let main (n:int(*-:{v:Int | true}*)) =
	assert (n <= sum n)