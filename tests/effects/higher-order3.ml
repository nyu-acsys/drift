let rec apply f x = (ev 1; if x <= 0 then 0 else ((apply f (f x))))

let pred x = x - 1
let succ x = x + 1

let main (n:int(*-:{v:Int | true}*)) =
  let _ = apply pred n in
  ev 2; 0