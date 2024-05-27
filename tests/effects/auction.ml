let rec refund j =
  if j = 0 then ()  
  else refund (j - 1)

let close n = ev 2; refund (n-1)

let rec bid i =
  if nondet then
    begin ev 1; bid (i + 1) end
  else
    close i

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 0
