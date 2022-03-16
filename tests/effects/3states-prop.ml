let init = 
  (0,0)

let trans q acc = 
  match (q,acc) with 
  | (0,_) -> 1
  | (1,_) -> 2
  | (2,_) -> 3
  | (3,_) -> 3

let final q acc = (q = 3)