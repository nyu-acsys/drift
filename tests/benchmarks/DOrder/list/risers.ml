let rec make_list m =
  if m <= 0
  then []
  else m :: make_list (m - 1)

let risersElse (x:int) xs = match xs with
    [] -> assert(false); []
  | s::ss -> 
    let ys = x :: [] in
    ys :: xs

let risersThen (x:int) xs = match xs with
    [] -> assert(false); []
  | s::ss -> let ys = x::s in ys ::ss

let rec risers (xs:int list) rn = match xs with
    [] -> []
  | x :: [] -> let ys = x :: [] in ys :: []
  | x::y::etc ->
       if (x > rn) then risersThen x (risers (y::etc))
			 else risersElse x (risers (y::etc))
				
let main (m:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  if m > 0 && n > 0 then
    let _ = risers (make_list m) n in
    ()
  else ()
