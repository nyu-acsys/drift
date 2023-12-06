
let rec compute vv bound inc = 
    if vv = bound then 0 else begin
      ev vv; 
      compute (inc vv) bound inc
    end

let main (v:int(*-:{v:Int | true}*)) (w:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  if (v>0) then
    let bound = -1 * v in
    compute v bound (fun t -> t-1)
  else 
    let bound = -1 * v in
    compute v bound (fun t -> t+1)
