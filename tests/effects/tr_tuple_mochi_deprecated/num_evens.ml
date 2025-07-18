(*
Tuple Encoding of Product Program.

Source Program: 

(* A program emits an event every alternate iteration of f.
  It makes sure that the overall number of effects is smaller
  than half of m. *)

let main (m:int(*-:{v:Int | v>0}*)) =
  let flip x =
    if x = 1 then 0
    else 1
  in
  let rec f m1 flag =
    if m1 = 0 then ()
    else
      (if flag=1 then begin ev 1; f (m1-1) end
      else f (m1-1))
      (flip flag)
  in
  f m 0


Property: 

QSet   = [0;1;1]; 

delta  = fun evx (q, acc) -> 
  if   (q = 0 && evx=1) then (0, acc+1)
  else (1, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> (q = 0 && (acc+acc <= prefm));

*)

let main (prefm:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if ((q = 0) && (evx = 1)) then (0,(acc + 1))
              else (1,acc))
in

let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((q = 0) && ((acc + acc) <= prefm))) 
in

if prefm >0 then
  let flip0 = ((fun x cfg12 ->
                  if (x = 1) then (0,cfg12)
                  else (1,cfg12)),(0,0)) in 
  (match flip0 with 
   (flip,cfg2) -> let f0 = ((let rec f m1 cfg5 =
                               ((fun flag cfg6 ->
                                   if (m1 = 0) then ((),cfg6)
                                   else
                                     (match if
                                              (flag = 1)
                                              then
                                              (match ((fun cfg7 ->
                                                         ((ev_step_asst0 cfg7) ; ((),cfg7))) ((ev_step0 1) cfg6)) with 
                                               (x1,cfg8) -> (match ((f (m1 - 1)) cfg8) with 
                                                             (x2,cfg9) -> ((x1 ; x2),cfg9)))
                                              else ((f (m1 - 1)) cfg6) with 
                                            (x3,cfg10) -> (match ((flip flag) cfg10) with 
                                                           (x4,cfg11) -> ((x3 x4) cfg11)))),cfg5)
                                     in f),cfg2) in 
                               (match f0 with 
                                (f,cfg3) -> (match ((f prefm) cfg3) with 
                                             (x0,cfg4) -> ((x0 0) cfg4))))
else
  ((), (0, 0))