(*
Tuple Encoding of Product Program.

Source Program: 

(* A program that chooses n numbers between 0 and m. 
  Each number is chosen between 0 and m non-deterministically.
  The property file checks if every chosen number is ≤ m. *)

let rec make_list n f =
  if n = 0 then 0
  else begin
    ev (f n);
    make_list (n-1) f
  end

let main (n:int(*-:{v:Int | v>0}*)) =
  let rec get_random_int n1 =
    if n1 = 0 then 0
    else
      if (Random.int(0) > 0) then get_random_int (n1-1)
      else n1
  in
  make_list n (get_random_int)


Property: 

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
  if   (q = 0 && evx >= acc) then (0, evx)
  else if   (q = 0) then (0, acc)
  else (q, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> q = 0 && acc <= prefn;


*)

let main (prefn:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if ((q = 0) && (evx >= acc)) then (0,evx)
              else (if (q = 0) then (0,acc)
                   else (q,acc))))
in

let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((q = 0) && (acc <= prefn)))
in

let rec make_list n f cfg2 =
  (if (n = 0) then (0,cfg2)
  else
    (match let cfg3 = ((ev_step0 (f n)) cfg2) in 
           ((ev_step_asst0 cfg3),cfg3) with 
     (x0,cfg4) -> (((make_list (n - 1)) f) cfg4))) 
in

  let rec get_random_int n1 =
    (if (n1 = 0) then 0
    else (if (Random.int(0) > 0) then (get_random_int (n1 - 1))
         else n1)) in 
    (((make_list prefn) get_random_int) (0,0))
