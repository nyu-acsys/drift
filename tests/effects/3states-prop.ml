let init = 
  (0,0)

let trans q_acc = 
  match q_acc with 
  | (0,_) -> 1
  | (1,_) -> 2
  | (2,_) -> 3
  | (3,_) -> 3

let final q_acc = match q_acc with | (q,acc) -> (q = 3)
 
let main =
  if final (trans init) then assert(true)