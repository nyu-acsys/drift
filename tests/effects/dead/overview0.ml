
let busy n t =
  ev 5; 0
(*
  if n <= 0 then begin 
    ev -t; 
    0
  end else busy (n - 1) t
*)

let main (x:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  busy n x
