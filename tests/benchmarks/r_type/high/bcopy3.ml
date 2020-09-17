let make_array n i = assert (0 <= i && i < n); 0
let update des i x j = if i=j then x else des i

let rec bcopy_aux m src des i =
  if i >= m
  then ()
  else
    let r =
      update des i (src i);
      bcopy_aux m src des (i+1)
    in r

let main (n(*-:{v:Int|true}*)) (m(*-:{v:Int|true}*)) =
  let array1 = make_array n in
  let array2 = make_array m in
  if n<=m then bcopy_aux n array1 array2 0 else ()
