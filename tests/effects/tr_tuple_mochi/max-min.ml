(*
Tuple Encoding of Product Program.

Source Program: 


let rec compute vv bound1 inc = 
    ev vv;
    if vv = bound1 then 0 else compute (inc vv) bound1 inc

let main (v:int(*-:{v:Int | true}*)) (w:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  let f = (fun t -> if v >= 0 then t-1 else t+1) in
  if (v>=0 && v <= 100000) then
    let bound = -v in
    compute v bound f
  else if (v<0 && v >= (-100000)) then
    let bound = -v in
    compute v bound f
  else
    0


Property: 

(* Max == -1 * Min *)
 
QSet   = [0;1]; 

delta  = fun evx (q, (max,min)) -> 
   if evx < min && evx > max then (1, (evx, evx))
   else if evx < min then (1, (max,evx)) 
   else if evx > max then (1, (evx,min)) 
   else (1, (max,min));

IniCfg = (0, (-100000,100000));

assertFinal = fun (q, (max,min)) -> max + min = 0;


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (max,min) -> if ((evx < min) && (evx > max)) then (1,(evx,evx))
                             else if (evx < min) then (1,(max,evx))
                                  else if (evx > max) then (1,(evx,min))
                                       else (1,(max,min))))


let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (max,min) -> assert ((max + min) = 0)))


let rec compute vv cfg2 =
  ((fun bound1 cfg3 ->
      ((fun inc cfg4 ->
          (match if (vv = bound1) then (0,((ev_step0 vv) cfg4))
                 else
                   (match (match (match ((inc vv) ((ev_step0 vv) cfg4)) with 
                                  (x0,cfg5) -> ((compute x0) cfg5)) with  (x1,cfg6) -> ((x1 bound1) cfg6)) with 
                           (x2,cfg7) -> ((x2 inc) cfg7)) with 
                   (x3,cfg8) -> ((() ; x3),cfg8))),cfg3)),cfg2) 


let main (v:int(*-:{cur_v:Int | true = true}*)) (w:int(*-:{cur_v:Int | true = true}*)) (m:int(*-:{cur_v:Int | true = true}*)) =
  (match let f0 = ((fun t (cfg16 : (int*(int*int))) ->
                      if (v >= 0) then ((t - 1),cfg16)
                      else ((t + 1),cfg16)),(0,((- 100000),100000))) in 
         (match f0 with 
          (f,cfg9) -> if
                        ((v >= 0) && (v <= 100000))
                        then
                        let bound1 = ((- v),cfg9) in 
                        (match bound1 with 
                         (bound,cfg13) -> (match (match ((compute v) cfg13) with 
                                                  (x6,cfg14) -> ((x6 bound) cfg14)) with  (x7,cfg15) -> ((x7 f) cfg15)))
                        else
                          if
                            ((v < 0) && (v >= (-100000)))
                            then
                            let bound0 = ((- v),cfg9) in 
                            (match bound0 with 
                             (bound,cfg10) -> (match (match ((compute v) cfg10) with 
                                                      (x4,cfg11) -> ((x4 bound) cfg11)) with  (x5,cfg12) -> ((x5 f) cfg12)))
                            else (0,cfg9)) with 
                      (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
