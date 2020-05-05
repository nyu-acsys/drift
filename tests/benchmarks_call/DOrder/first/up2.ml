

let rec loopa ai ak an = 
    if ai < an then
        loopa (ai + 1) (ak + 1) an
    else ak

let rec loopb bj bk bn = 
    if bj < bn then
        if bk > 0 then
            loopb (bj + 2) (bk - 1) bn
        else -1
    else bk

let main_p (n:int) =
  if n > 0 then
    let i = 0 in
    let k = 0 in
    let ans = loopa i k n in
    let j = 0 in
    assert(loopb j ans n <= n)
  else ()

let main (w:unit) =
	let _ = main_p 9 in
    let _ = main_p 10 in
    let _ = main_p (-5) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()