

let rec loopa ai ak an = 
    if ai < an then
        loopa (ai + 1) (ak + 1) an
    else ak
in

let rec loopb bj bk bn = 
    if bj < bn then
        if bk > 0 then
            loopb (bj + 2) (bk - 1) bn
        else -1
    else bk
in

let main n =
  let i = 0 in
  let k = 0 in
  let ans = loopa i k n in
  let j = 0 in
  assert(loopb j ans n <= n)
in main 100