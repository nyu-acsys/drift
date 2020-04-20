

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

let main (n:int(*-:{v:Int | true}*)) =
  if n > 0 then
    let i = 0 in
    let k = 0 in
    let ans = loopa i k n in
    let j = 0 in
    assert(loopb j ans n <= n)
  else assert(true)

let _ = main 10
let _ = main 9
let _ = main (-5)