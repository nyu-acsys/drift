type q = Q0 | Q1 | Q2 | Q3
type tup = Tup of q * int

let rec ev d acc =
  match acc with
  | Tup(Q0,_) -> Tup(Q1,1)
  | Tup(Q1,_) -> Tup(Q2,1)
  | _ -> Tup(Q3,1)

(* at least 3 events happen, then only "v" events *)
let rec aut n _s0 =
   if n > 0 then begin
      let _s1 = ev n _s0 in
      aut (n-1) _s1 
  end else _s0

let main (n:int(*-:{v:Int | true}*)) = 
  let _s00 = Tup(Q0,0) in
  if n > 5 then begin
    let _s01 = aut n _s00 in
    match _s01 with
    | Tup(Q0,_) -> assert(false)
    | Tup(Q1,_) -> assert(false)
    | Tup(Q2,_) -> assert(false)
    | _ -> ()
  end