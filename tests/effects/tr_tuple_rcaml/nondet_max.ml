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
      if nondet then get_random_int (n1-1)
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

external nondet_bool : unit -> bool = "unknown"

let main (prefn:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if ((q = 0) && (evx >= acc)) then (0,evx)
              else if (q = 0) then (0,acc)
                   else (q,acc))
in

let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((q = 0) && (acc <= prefn)))
in

let rec make_list n cfg2 =
  ((fun f cfg3 ->
      if (n = 0) then (0,cfg3)
      else
        (match (match ((f n) cfg3) with 
                (x0,cfg5) -> ((fun cfg4 ->
                                 ((ev_step_asst0 cfg4) ; ((),cfg4))) ((ev_step0 x0) cfg5))) with 
               (x1,cfg6) -> (match (match ((make_list (n - 1)) cfg6) with 
                                    (x3,cfg8) -> ((x3 f) cfg8)) with  (x2,cfg7) -> ((x1 ; x2),cfg7)))),cfg2) 
in

  let get_random_int0 = ((let rec get_random_int n1 cfg11 =
                            if (n1 = 0) then (0,cfg11)
                            else if (nondet_bool ()) then ((get_random_int (n1 - 1)) cfg11)
                                 else (n1,cfg11)
                            in get_random_int),(0,0)) in 
  (match get_random_int0 with 
   (get_random_int,cfg9) -> (match ((make_list prefn) cfg9) with 
                             (x4,cfg10) -> ((x4 get_random_int) cfg10)))

[@@@assert "typeof(main) <: int -> int * (int * int)"]
