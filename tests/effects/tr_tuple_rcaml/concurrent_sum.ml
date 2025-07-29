(*
Tuple Encoding of Product Program.

Source Program: 


let choose x =
  if (nondet_bool ()) then begin
    ev 1;
    0
  end else if (nondet_bool ()) then begin
    ev 2;
    0
  end else begin
    ev 3;
    0
  end

let rec sum n =
  if n = 0 then 0
  else begin
    choose ();
    sum (n-1) end

let main (n:int(*-:{v:Int | v>0}*)) = 
  sum n


Property: 

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
  if   (q = 0 && evx <= 3) then (0, evx + acc)
  else (1, acc);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 0 && acc <= 3*prefn;


*)

external nondet_bool : unit -> bool = "unknown"

let main (prefn:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if ((q = 0) && (evx <= 3)) then (0,(evx + acc))
              else (1,acc)))
in

let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((q = 0) && (acc <= (3 * prefn))))
in

let choose x cfg2 =
  (if (nondet_bool ()) then ((() ; 0),((ev_step0 1) cfg2))
  else (if (nondet_bool ()) then ((() ; 0),((ev_step0 2) cfg2))
       else ((() ; 0),((ev_step0 3) cfg2))))
in

let rec sum n cfg3 =
  (if (n = 0) then (0,cfg3)
  else (match ((choose ()) cfg3) with 
        (x0,cfg4) -> ((sum (n - 1)) cfg4))) 
in

if prefn > 0 then
  (match ((sum prefn) (0,0)) with 
   (e0,cfg6) -> ((asst_final0 cfg6) ; e0))
else
  0

[@@@assert "typeof(main) <: int -> int"]
