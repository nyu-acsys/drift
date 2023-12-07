(*
let intermediaAssertion v q acc = 
  q = 0
[@@@assert "typeof(intermediateAssertion) <: ... -> { ret: bool | ret = true }"]
*)
let ev v cfg = 
  match cfg with 
  | (q,acc) -> (q, acc)

let foo x = 
  match (1,(0,0)) with
  | (e0, acfg) ->
    (fun cfg2 -> (true,cfg2) ) (acfg)

    (* (1,(0,1)) *)

(* 
(fun ev ->
     match (1,(0,0)) with
     | (e0, acfg) ->
       (fun cfg2 -> (true,cfg2) ) (acfg)
  ) (fun evx -> 
      fun cfg ->
            match cfg with 
            | (q,acc) -> (q, acc)
  ) *)

let bar (x:int) : bool = 
  match foo x with
  | (v,(q,acc)) -> q>0 (* q>0 put the final assertion here *)

[@@@assert "typeof(bar) <: (x:{ x:int | true }) -> { ret : bool | ret = true }"]

(*
App (Rec (lambda ev_0^0.
           PatMat (match (Const 0^1, (Const 0^2, Const 0^3)^4)^5 with
                    (Var e0^6, Var acfg0^7)^8 ->
                      App (Rec (lambda cfg2^9. Const true^10)^11
                            Var acfg0^12)^13)^14)^15
      Rec (lambda evx^16.
            Rec (lambda cfg0^17.
                  PatMat (match Var cfg0^18 with
                           (Var q^19, Var acc^20)^21 ->
                             (Var q^22, Var acc^23)^24)^25)^26)^27)^28
*)