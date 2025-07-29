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

let main (prefgl_t:int(*-:{cur_v:Int | true = true}*)) (prefgl_d:int(*-:{cur_v:Int | true = true}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (q,(acc + evx)))
in

let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (acc = (prefgl_t / prefgl_d)))
in

let rec shrink t f d cfg2 =
  (if ((f ()) <= 0) then (0,cfg2)
  else let t_pm = ((f ()) - d) in 
       ((((shrink t_pm) (fun x ->
                           t_pm)) d) ((ev_step0 1) cfg2))) 
in

  (match ((((shrink prefgl_t) (fun x ->
                             prefgl_t)) prefgl_d) ((ev_step0 (prefgl_t / prefgl_d)) (0,0))) with 
   (e0,cfg4) -> ((asst_final0 cfg4) ; e0))

[@@@assert "typeof(main) <: int -> int -> int"]
