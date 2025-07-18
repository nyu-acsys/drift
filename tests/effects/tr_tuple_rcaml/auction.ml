(*
Tuple Encoding of Product Program.

Source Program: 

let refund k kamt h _ = 
  if k <= 1 then ()
  else begin ev 3; h () end

let close j g = 
  if j = 1 then ()
  else begin ev 2; g () end

let rec bid i amtW f =
  let amt = amtW + 1 in
  if (nondet_bool ()) then 
    begin ev 1; bid (i + 1) amt (refund i amtW f) end
  else
    close i f

let idf _ = ()

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 1 1 idf


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
	  else if (q = 1 && evx = 3 && (bidders > refunds + 1)) then (1, (bidders, refunds+1))
	  else (2, (bidders, refunds));

IniCfg = (0, (0,0));

assert = fun (q, (bidders, refunds)) -> q < 2;

assertFinal = fun (q, (bidders, refunds)) -> (q = 0) || (q = 1 && refunds = bidders - 1);


*)

external nondet_bool : unit -> bool = "unknown"

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (bidders,refunds) -> (if ((q = 0) && (evx = 1)) then (q,((bidders + 1),refunds))
                                     else
                                       (if ((q = 0) && (evx = 2)) then (1,(bidders,refunds))
                                       else
                                         (if
                                            (((q = 1) && (evx = 3)) && (bidders > (refunds + 1)))
                                            then
                                            (1,(bidders,(refunds + 1)))
                                         else (2,(bidders,refunds)))))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (bidders,refunds) -> assert (q < 2)))


let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc2) -> (match acc2 with 
                (bidders,refunds) -> assert ((q = 0) || ((q = 1) && (refunds = (bidders - 1))))))


let refund k kamt h _ cfg3 =
  (if (k <= 1) then ((),cfg3)
  else (match let cfg4 = ((ev_step0 3) cfg3) in 
              ((ev_step_asst0 cfg4),cfg4) with 
        (x0,cfg5) -> ((h ()) cfg5)))


let close j g cfg7 =
  (if (j = 1) then ((),cfg7)
  else (match let cfg8 = ((ev_step0 2) cfg7) in 
              ((ev_step_asst0 cfg8),cfg8) with 
        (x2,cfg9) -> ((g ()) cfg9)))


let rec bid i amtW f cfg11 =
  let amt = (amtW + 1) in 
  (if
     (nondet_bool ())
     then
     (match let cfg12 = ((ev_step0 1) cfg11) in 
            ((ev_step_asst0 cfg12),cfg12) with 
      (x4,cfg13) -> ((((bid (i + 1)) amt) (((refund i) amtW) f)) cfg13))
     else (((close i) f) cfg11))


let idf _ cfg15 =
  ((),cfg15) 


let main (u:unit(*-:{cur_v:Unit | unit = unit}*)) =
  (match ((((bid 1) 1) idf) (0,(0,0))) with 
   (e0,cfg16) -> ((asst_final0 cfg16) ; e0))

[@@@assert "typeof(main) <: unit -> unit"]
