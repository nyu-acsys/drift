(*
Tuple Encoding of Product Program.

Source Program: 

(* Taken from
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10

let g i x = x-i

let app f x i =
   event "P"; (* added to capture the fact that P happened before *)
   f x (g i)

let rec ha1 (x:int) =
   event "P"; (* added to capture the fact that P happened before *)
(*   event "Ha1";*)
   event "Ha";
   ha1 x (* added to turn "Ha1 happens" to "Ha1 happens infinitely often" *)

let rec ha2 (x:int) =
   event "P"; (* added to capture the fact that P happened before *)
(*   event "Ha2"; *)
   event "Ha";
   ha2 x (* added to turn "Ha2 happens" to "Ha2 happens infinitely often" *)

let rec walk x f =
(*  event "Walk";*)
  event "P"; (* added to capture the fact that P happened before *)
  if x<0 then x
  else walk (f x) f

let rec run x f =
(*  event "Run";*)
  event "P"; (* added to capture the fact that P happened before *)
  if x<0 then x
  else run (f(f x)) f

let rec life x =
  if Random.int(0)>0 then
    (event "P";
     if x<0 then ha1 (app walk x (1))
     else ha2 (app run x 1))
  else life x

let main() = life (Random.int 0) *)

(* Property to be checked: if event P occurs infinitely often so does Ha *)
(* The original property described in their paper is
   G(P=>X (Walk U Ha1 / Run U Ha2))
   this can be expressed as the conjunction of the following properties:
    1. If P happens, then Ha1 or Ha2 happens eventually
    2. The next event after P is one of the events in {Ha1,Walk,Ha2,Run}
    3. After P and Walk happens, the next action is Walk or Ha1.
    4. After P and Run happens, the next action is Run or Ha2.
   The properties 2-4 are safety properties,
   which can be checked by plain MoCHi.

{SPEC}
   fairness: (Always, P)  (* either P does not happen, or *)
   fairness: (Ha, Never) (* Ha happens eventually *)
{SPEC}*)

let step i j  = j - i

let app h x1 x2 = h x1 (step x2)

let ha1 b = ev 3
  
let ha2 a = ev 5
  
let rec walk z g = 
  begin
    ev 2;
    if z < 0 then z
    else walk (g z) g
  end

let rec run y f = 
  begin
    ev 4;
    if y < 0 then y
    else run (f (f y)) f
  end

let rec life x = 
  if nondet then 
    begin 
      ev 1;
      if x < 0 then ha1 (app walk x 1)
      else ha2 (app run x 1)
    end
  else
    life x

let main (v: int(*-:{v: Int | true}*)) = 
  life v


Property: 

(* 
 * Property to be checked (as described in the paper):
 *  P => X (Walk U Ha1 \/ Run U Ha2)
 * 
 * encoding of events: P(1), Walk(2), Ha1(3), Run(4), Ha(5)
 * 
 * Property automaton: tracks the expected order of events
 * 
 * (q0)--[P]-->(q1)
 * (q1)--[P]-->(q1)
 * (q1)--[Walk]-->(q2)
 * (q2)--[Walk]-->(q2)
 * (q2)--[Ha1]-->(q3)
 * (q1)--[Run]-->(q4)
 * (q4)--[Run]-->(q4)
 * (q4)--[Ha2]-->(q5)

 * (q6) is the error sink state
 *)

QSet = [0; 1; 2; 3; 4; 5; 6];

delta = fun evx (q, acc) ->
          if (q = 0 && evx = 1) then (1, acc)
	  else if (q = 1 && evx = 1) then (1, acc)
	  else if (q = 1 && evx = 2) then (2, acc)
	  else if (q = 2 && evx = 2) then (2, acc)
	  else if (q = 2 && evx = 3) then (3, acc)
	  else if (q = 1 && evx = 4) then (4, acc)
	  else if (q = 4 && evx = 4) then (4, acc)
	  else if (q = 4 && evx = 5) then (5, acc)
	  else (6, acc);
	  
IniCfg = (0, 0);

assert = fun (q, acc) -> q < 6;

assertFinal = fun (q, acc) -> (q = 3) || (q = 5);


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if ((q = 0) && (evx = 1)) then (1,acc)
              else
                if ((q = 1) && (evx = 1)) then (1,acc)
                else
                  if ((q = 1) && (evx = 2)) then (2,acc)
                  else
                    if ((q = 2) && (evx = 2)) then (2,acc)
                    else
                      if ((q = 2) && (evx = 3)) then (3,acc)
                      else if ((q = 1) && (evx = 4)) then (4,acc)
                           else if ((q = 4) && (evx = 4)) then (4,acc)
                                else if ((q = 4) && (evx = 5)) then (5,acc)
                                     else (6,acc))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q < 6))


let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc) -> assert ((q = 3) || (q = 5)))


let step i cfg3 =
  ((fun j cfg4 ->
      ((j - i),cfg4)),cfg3)


let app h cfg5 =
  ((fun x1 cfg6 ->
      ((fun x2 cfg7 ->
          (match ((h x1) cfg7) with 
           (x0,cfg8) -> (match ((step x2) cfg8) with 
                         (x1,cfg9) -> ((x0 x1) cfg9)))),cfg6)),cfg5)


let ha1 b cfg10 =
  ((fun cfg11 ->
      ((ev_step_asst0 cfg11) ; ((),cfg11))) ((ev_step0 3) cfg10))


let ha2 a cfg12 =
  ((fun cfg13 ->
      ((ev_step_asst0 cfg13) ; ((),cfg13))) ((ev_step0 5) cfg12))


let rec walk z cfg14 =
  ((fun g cfg15 ->
      (match ((fun cfg16 ->
                 ((ev_step_asst0 cfg16) ; ((),cfg16))) ((ev_step0 2) cfg15)) with 
       (x2,cfg17) -> (match if (z < 0) then (z,cfg17)
                            else (match (match ((g z) cfg17) with 
                                         (x4,cfg19) -> ((walk x4) cfg19)) with  (x5,cfg20) -> ((x5 g) cfg20)) with 
                            (x3,cfg18) -> ((x2 ; x3),cfg18)))),cfg14)


let rec run y cfg21 =
  ((fun f cfg22 ->
      (match ((fun cfg23 ->
                 ((ev_step_asst0 cfg23) ; ((),cfg23))) ((ev_step0 4) cfg22)) with 
       (x6,cfg24) -> (match if (y < 0) then (y,cfg24)
                            else
                              (match (match (match ((f y) cfg24) with 
                                             (x8,cfg26) -> ((f x8) cfg26)) with  (x9,cfg27) -> ((run x9) cfg27)) with 
                                      (x10,cfg28) -> ((x10 f) cfg28)) with 
                              (x7,cfg25) -> ((x6 ; x7),cfg25)))),cfg21)


let rec life x cfg29 =
  if
    (Random.int(0) > 0)
    then
    (match ((fun cfg30 ->
               ((ev_step_asst0 cfg30) ; ((),cfg30))) ((ev_step0 1) cfg29)) with 
     (x11,cfg31) -> (match if
                             (x < 0)
                             then
                             (match (match (match ((app walk) cfg31) with 
                                            (x16,cfg36) -> ((x16 x) cfg36)) with  (x17,cfg37) -> ((x17 1) cfg37)) with 
                                     (x18,cfg38) -> ((ha1 x18) cfg38))
                              else
                                (match (match (match ((app run) cfg31) with 
                                               (x13,cfg33) -> ((x13 x) cfg33)) with  (x14,cfg34) -> ((x14 1) cfg34)) with 
                                        (x15,cfg35) -> ((ha2 x15) cfg35)) with 
                                (x12,cfg32) -> ((x11 ; x12),cfg32)))
                           else ((life x) cfg29) 


let main (v:int(*-:{cur_v:Int | true = true}*)) =
  (match ((life v) (0,0)) with 
   (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
