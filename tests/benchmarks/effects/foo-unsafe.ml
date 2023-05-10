type q = Qerr | Q0
type tup = Tup of q * int
let rec ev d acc =
  match acc with
  | Tup(Qerr,_) -> acc
  | Tup(Q0,_) -> (if d > 0 then (Tup(Qerr,d)) else (Tup(Q0,d)))

let rec foo x acc = 
   let acc' = ev x acc in
   if x = 0 then acc' else (  foo (x-1) acc' )

let main () = 
   let acc' = foo 10 (Tup(Q0,0)) in
   match acc' with
   | Tup(Qerr,_) -> assert(false)
   | _ -> ()
