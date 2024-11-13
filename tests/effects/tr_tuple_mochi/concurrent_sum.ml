(*
Tuple Encoding of Product Program.

Source Program: 


let choose x =
  if nondet then begin
    ev 1;
    0
  end else if nondet then begin
    ev 2;
    0
  end else begin
    ev 3;
    0
  end

let rec sum n =
  if n = 0 then 0
  else begin
    (* if nondet then *)
    (*   ev 1 *)
    (* else if nondet then *)
    (*   ev 2 *)
    (* else *)
    (*   ev 3; *)
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

let main (prefn:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if ((q = 0) && (evx <= 3)) then (0,(evx + acc))
              else (1,acc))
in

let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((q = 0) && (acc <= (3 * prefn))))
in

let choose x cfg2 =
  if (Random.int(0) >= 0) then ((() ; 0),((ev_step0 1) cfg2))
  else if (Random.int(0) >= 0) then ((() ; 0),((ev_step0 2) cfg2))
       else ((() ; 0),((ev_step0 3) cfg2))
in

let rec sum n (cfg3 : (int * int)) =
  if (n = 0) then (0,cfg3)
  else (match ((choose ()) cfg3) with 
        (x0,cfg4) -> (match ((sum (n - 1)) cfg4) with 
                      (x1,cfg5) -> ((x0 ; x1),cfg5))) 
in

  (match ((sum prefn) (0,0)) with 
   (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
