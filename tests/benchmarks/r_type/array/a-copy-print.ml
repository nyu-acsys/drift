(*
USED: PLDI2011 as a-cppr
USED: PEPM2013 as a-cppr
*)
let print_int pn = () in

let rec bcopy bm srcb desb bi =
  if bi >= bm then
    desb
  else
    (set desb bi (get srcb bi);
    bcopy bm srcb desb (bi + 1))
in

let rec print_array pm ary pi =
  if pi >= pm then
    ()
  else
    (print_int (get ary pi);
     print_array pm ary (pi + 1))
in

let f m src des =
  let res = bcopy m src des 0 in
  print_array m res 0
in

let main n =
  let array1 = make n 0 in
  let array2 = make n 1 in
    if n > 0 then f n array1 array2 else ()
in 
main 10