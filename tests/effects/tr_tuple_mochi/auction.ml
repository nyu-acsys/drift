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
  if nondet then 
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

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (bidders,refunds) -> if ((q = 0) && (evx = 1)) then (q,((bidders + 1),refunds))
                                     else
                                       if ((q = 0) && (evx = 2)) then (1,(bidders,refunds))
                                       else if (((q = 1) && (evx = 3)) && (bidders > (refunds + 1))) then (1,(bidders,(refunds + 1)))
                                            else (2,(bidders,refunds))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (bidders,refunds) -> assert (q < 2)))


let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc2) -> (match acc2 with 
                (bidders,refunds) -> assert ((q = 0) || ((q = 1) && (refunds = (bidders - 1))))))


let refund k cfg3 =
  ((fun kamt cfg4 ->
      ((fun h cfg5 ->
          ((fun _ cfg6 ->
              if (k <= 1) then ((),cfg6)
              else (match ((fun cfg7 ->
                              ((ev_step_asst0 cfg7) ; ((),cfg7))) ((ev_step0 3) cfg6)) with 
                    (x0,cfg8) -> (match ((h ()) cfg8) with 
                                  (x1,cfg9) -> ((x0 ; x1),cfg9)))),cfg5)),cfg4)),cfg3)


let close j cfg10 =
  ((fun g cfg11 ->
      if (j = 1) then ((),cfg11)
      else (match ((fun cfg12 ->
                      ((ev_step_asst0 cfg12) ; ((),cfg12))) ((ev_step0 2) cfg11)) with 
            (x2,cfg13) -> (match ((g ()) cfg13) with 
                           (x3,cfg14) -> ((x2 ; x3),cfg14)))),cfg10)


let rec bid i cfg15 =
  ((fun amtW cfg16 ->
      ((fun f cfg17 ->
          let amt0 = ((amtW + 1),cfg17) in 
          (match amt0 with 
           (amt,cfg18) -> if
                            (Random.int(0) > 0)
                            then
                            (match ((fun cfg20 ->
                                       ((ev_step_asst0 cfg20) ; ((),cfg20))) ((ev_step0 1) cfg18)) with 
                             (x5,cfg21) -> (match (match (match ((bid (i + 1)) cfg21) with 
                                                          (x7,cfg23) -> ((x7 amt) cfg23)) with 
                                                         (x8,cfg24) -> (match (match (match ((refund i) cfg24) with 
                                                                                      (x9,cfg25) -> ((x9 amtW) cfg25)) with  (x10,cfg26) -> ((x10 f) cfg26)) with 
                                                                               (x11,cfg27) -> ((x8 x11) cfg27))) with 
                                                         (x6,cfg22) -> ((x5 ; x6),cfg22)))
                                            else (match ((close i) cfg18) with 
                                                  (x4,cfg19) -> ((x4 f) cfg19)))),cfg16)),cfg15)


let idf _ cfg28 =
  ((),cfg28) 


let main (u:unit(*-:{cur_v:Unit | unit = unit}*)) =
  (match (match (match ((bid 1) (0,(0,0))) with 
                 (x12,cfg29) -> ((x12 1) cfg29)) with  (x13,cfg30) -> ((x13 idf) cfg30)) with 
          (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
