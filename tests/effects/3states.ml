(* simple 3-state automaton, the final state accepting. *)
let ev d _s = 
    if _s = 0 then 1
    else if _s = 1 then 2
    else if _s = 2 then 3
    else -1

let nondet (i:int) = (i * i * i * 500) mod 199

(* at least 3 events happen, then only "v" events *)
let rec aut n _s0 =
   if n > 0 then begin
      let _s1 = ev n _s0 in
      aut (n-1) _s1 
  end else _s0

let main (n:int(*-:{v:Int | true}*)) = 
  let _s00 = 0 in
  if n > 5 then begin
    let _s01 = aut n _s00 in
    assert (_s01 < 0)
  end