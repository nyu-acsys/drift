
let rec compute vv bound1 inc = 
    ev vv;
    if vv = bound1 then 0 else compute (inc vv) bound1 inc

let main (v:int(*-:{v:Int | true}*)) (w:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  let f = (fun t -> if v >= 0 then t-1 else t+1) in
  if (v>=0 && v <= 100000) then
    let bound = -v in
    compute v bound f
  else if (v<0 && v >= (-100000)) then
    let bound = -v in
    compute v bound f
  else
    0
