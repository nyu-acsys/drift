(*
USED: PLDI2011 as a-cppr
USED: PEPM2013 as a-cppr
*)
let print_int n = () in
let f m src des =
  let rec bcopy m src des i =
    if i >= m then
      des
    else
      set des i (get src i);
      bcopy m src des (i+1)
  in
  let rec print_array m ary i =
    if i >= m then
      ()
    else
      (print_int (get ary i);
       print_array m ary (i + 1))
  in
  let res = bcopy m src des 0 in
    print_array m res 0
in
let main n =
  let array1 = make n 0 in
  let array2 = make n 1 in
    if n > 0 then f n array1 array2 else ()
in 
main 3