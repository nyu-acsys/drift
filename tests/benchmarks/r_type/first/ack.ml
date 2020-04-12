(*
USED: PLDI2011 as ack
USED: PEPM2013 as ack
*)

let main (mm(*-:{v:Int | v >= 0}*)) (mn(*-:{v:Int | v >= 0}*)) =
    let rec ack m n =
      if m = 0 then n + 1
      else if n = 0 then ack (m - 1) 1
      else ack (m - 1) (ack m (n - 1))
    in

    assert(ack mm mn >= mn)
(* in assert( main 3 2 = true) *)