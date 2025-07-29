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

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
  if   (q = 0 && evx=1) then (0, acc+1)
  else (1, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> (q = 0 && (2*acc <= prefm));

*)

let main (m:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if ((q = 0) && (evx = 1)) then (0,(acc + 1))
              else (1,acc)))
in


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((q = 0) && ((2 * acc) <= m))) 
in


  let flip = (fun x ->
                (if (x = 1) then 0
                else 1)) in 
  let rec f
    m1 =
    (fun flag -> fun cfg2 ->
       (if (m1 = 0) then ((),cfg2)
       else
         (match (if
                   (flag = 1)
                   then
                   (match let cfg3 = ((ev_step0 1) cfg2) in 
                          ((ev_step_asst0 cfg3),cfg3) with 
                    (x0,cfg4) -> ((x0 ; (f (m1 - 1))),cfg4))
                   else ((f (m1 - 1)),cfg2)) with 
                (x1,cfg5) -> ((x1 (flip flag)) cfg5)))) in 
       (((f m) 0) (0,0))