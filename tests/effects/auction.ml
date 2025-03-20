let refund k kamt h _ = 
  if k <= 1 then ()
  else begin ev 3; h () end

let close j g = 
  if j = 1 then ()
  else begin ev 2; g () end

let rec bid i amtW f =
  let amt = amtW + 1 in
  if nondet then 
    begin ev 1; bid (i + 1) amt (refund i amtW f) end
  else
    close i f

let idf _ = ()

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 1 1 idf
