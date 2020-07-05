let rec mem (x:int) (xs:int list) =
  match xs with
      [] -> 0
    | x'::xs' -> 
			if x = x' then 1
			else if (mem x xs' > 0) then 1
			else 0

let rec make_list n (x:int) =
  if n < 0
  then []
  else x :: make_list (n - 1) x

let main (n:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  if n > 0 then
    let xs = make_list n m in
  	match xs with
  		| [] -> ()
  		| x::xs' -> assert (mem m xs > 0)
  else ()