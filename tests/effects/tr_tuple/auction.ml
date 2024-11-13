(*
Tuple Encoding of Product Program.

Source Program: 

let rec refund j =
  if j <= 0 then ()  
  else begin ev 3; refund (j - 1) end

let close n = 
  if n=0 then () 
  else begin ev 2; refund (n-1) end

let rec bid i =
  if nondet then
    begin ev 1; bid (i + 1) end
  else
    close i

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 0


Property: 

(* 
 * Abstraction of an auction smart contract running on blockchain:
 * "The property states that if N users place a bid and the auction has ended, then there will be N-1 successful refunds."
 * 
 * encoding of events: bid(1), close(2), refund(3)
 * 
 * Automaton property
 *   keep track of number of bids 
 * 
 * (q0)--[bid; bidders++]-->(q0)
 * (q0)--[close]-->(q1)
 * (q1)--[refund; refunds--]-->(q1)
 *)

QSet = [0; 1; 2];

delta = fun evx (q, (bidders, refunds)) ->
          if (q = 0 && evx = 1) then (q, (bidders+1, refunds))
	  else if (q = 0 && evx = 2) then (1, (bidders, refunds))
	  else if (q = 1 && evx = 3) then (1, (bidders, refunds+1))
	  else (2, (bidders, refunds));

IniCfg = (0, (0,0));

assert = fun (q, (bidders, refunds)) -> q < 2;

assertFinal = fun (q, (bidders, refunds)) -> (q = 0) || (q = 1 && refunds = bidders - 1);


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (bidders,refunds) -> if ((q = 0) && (evx = 1)) then (q,((bidders + 1),refunds))
                                     else if ((q = 0) && (evx = 2)) then (1,(bidders,refunds))
                                          else if ((q = 1) && (evx = 3)) then (1,(bidders,(refunds + 1)))
                                               else (2,(bidders,refunds))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (bidders,refunds) -> assert (q < 2)))


let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc2) -> (match acc2 with 
                (bidders,refunds) -> assert ((q = 0) || ((q = 1) && (refunds = (bidders - 1))))))


let rec refund j cfg3 =
  if (j <= 0) then ((),cfg3)
  else (match ((fun cfg4 ->
                  ((ev_step_asst0 cfg4) ; ((),cfg4))) ((ev_step0 3) cfg3)) with 
        (x0,cfg5) -> (match ((refund (j - 1)) cfg5) with 
                      (x1,cfg6) -> ((x0 ; x1),cfg6)))


let close n cfg7 =
  if (n = 0) then ((),cfg7)
  else (match ((fun cfg8 ->
                  ((ev_step_asst0 cfg8) ; ((),cfg8))) ((ev_step0 2) cfg7)) with 
        (x2,cfg9) -> (match ((refund (n - 1)) cfg9) with 
                      (x3,cfg10) -> ((x2 ; x3),cfg10)))


let rec bid i cfg11 =
  if
    (Random.int(0) >= 0)
    then
    (match ((fun cfg12 ->
               ((ev_step_asst0 cfg12) ; ((),cfg12))) ((ev_step0 1) cfg11)) with 
     (x4,cfg13) -> (match ((bid (i + 1)) cfg13) with 
                    (x5,cfg14) -> ((x4 ; x5),cfg14)))
    else ((close i) cfg11) 


let main (u:unit(*-:{cur_v:Unit | unit = unit}*)) =
  (match ((bid 0) (0,(0,0))) with 
   (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
