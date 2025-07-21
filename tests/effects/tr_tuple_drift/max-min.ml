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
                (max,min) -> (if ((evx < min) && (evx > max)) then (1,(evx,evx))
                             else
                               (if (evx < min) then (1,(max,evx))
                               else (if (evx > max) then (1,(evx,min))
                                    else (1,(max,min)))))))


let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (max,min) -> assert ((max + min) = 0)))


let rec compute vv bound1 inc cfg2 =
  (if (vv = bound1) then (0,((ev_step0 vv) cfg2))
  else ((((compute (inc vv)) bound1) inc) ((ev_step0 vv) cfg2))) 


let main (v:int(*-:{v:Int | true}*)) (w:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  (match let f = (fun t ->
                    (if (v >= 0) then (t - 1)
                    else (t + 1))) in 
         (if ((v >= 0) && (v <= 100000)) then let bound = (- v) in 
                                              ((((compute v) bound) f) (0,((- 100000),100000)))
         else
           (if
              ((v < 0) && (v >= (-100000)))
              then
              let bound = (- v) in 
              ((((compute v) bound) f) (0,((- 100000),100000)))
           else (0,(0,((- 100000),100000))))) with 
   (e0,cfg4) -> ((asst_final0 cfg4) ; e0))