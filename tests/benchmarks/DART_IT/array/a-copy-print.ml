(*
USED: PLDI2011 as a-cppr
USED: PEPM2013 as a-cppr
*)

let print_int (pn:int) = ()

let rec bcopy bm (srcb: int array) (desb: int array) bi =
  if bi >= bm then
    desb
  else
    (Array.set desb bi (Array.get srcb bi);
    bcopy bm srcb desb (bi + 1))

let rec print_array pm (ary: int array) pi =
  if pi >= pm then
    ()
  else
    (print_int (Array.get ary pi);
     print_array pm ary (pi + 1))

let f m (src: int array) (des: int array) =
  let res = bcopy m src des 0 in
  print_array m res 0

let main (mn:int(*-:{v:Int | true}*)) =
  if mn > 0 then
    let array1 = Array.make mn 0 in
    let array2 = Array.make mn 1 in
    f mn array1 array2
  else ()

let _ = main 3