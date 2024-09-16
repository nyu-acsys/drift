(*
Tuple Encoding of Product Program.

Source Program: 


let rec shrink t f d =
  if f () <= 0 then 0 
  else begin
    ev 1; (* ev[Shrink] *)
    let t' = f() - d in
    shrink t' (fun x -> t') d end

let main (gl_t:int(*-:{v:Int | true}*)) (gl_d:int(*-:{v:Int | true}*)) =   
    ev (gl_t/gl_d);
    shrink gl_t (fun x -> gl_t) gl_d


Property: 

(* Dependent *)
 
QSet   = [0]; 
 
delta  = fun evx (q, acc) -> (q, (acc+evx));
 
IniCfg = (0, 0);

assertFinal = fun (q, acc) -> (acc = prefgl_t/prefgl_d);
 


*)

let main (prefgl_t:int) (prefgl_d:int) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (q,(acc + evx)))
in

let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (acc = (prefgl_t / prefgl_d)))
in

let rec shrink t cfg2 =
  ((fun f cfg3 ->
      ((fun d cfg4 ->
          (match (match ((f ()) cfg4) with 
                  (x0,cfg5) -> (match (0,cfg5) with 
                                (x1,cfg6) -> ((x0 <= x1),cfg6))) with 
                  (x2,cfg7) -> if x2 then (0,cfg7)
                               else
                                 (match let t_pm0 = (match ((f ()) ((ev_step0 1) cfg7)) with 
                                                     (x5,cfg12) -> (match (d,cfg12) with 
                                                                    (x6,cfg13) -> ((x5 - x6),cfg13))) in 
                                                     (match t_pm0 with 
                                                      (t_pm,cfg8) -> (match (match ((shrink t_pm) cfg8) with 
                                                                             (x3,cfg9) -> ((x3 (fun x cfg10 ->
                                                                                                  (t_pm,cfg10))) cfg9)) with 
                                                                            (x4,cfg11) -> ((x4 d) cfg11))) with 
                                                     (x7,cfg14) -> ((() ; x7),cfg14)))),cfg3)),cfg2) 
in

  (match 
     (match 
        (match 
           (match ((shrink prefgl_t) ((ev_step0 (prefgl_t / prefgl_d)) (0,0))) with 
              (x8,cfg15) -> ((x8 (fun x cfg16 ->
                                 (prefgl_t,cfg16))) cfg15)) with  
           (x9,cfg17) -> ((x9 prefgl_d) cfg17)) with 
        (x10,cfg18) -> ((() ; x10),cfg18)) with 
     (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
