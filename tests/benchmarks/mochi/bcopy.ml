
let sub src i = assert (0 <= i && i < (len src)); 0 in

let rec bcopy_aux src des i m =
  if i >= m
  then ()
  else
    (
      set des i (sub src i);
      bcopy_aux src des (i+1) m
    )
in

let bcopy src des = bcopy_aux src des 0 (len src) in

let main n m =
  let array1 = make n 0 in
  let array2 = make m 0 in
  if n<=m then bcopy array1 array2 else ()
in
main 2 4
