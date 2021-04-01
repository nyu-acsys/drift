(* Taken from 
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10 *)

let rec halt (): unit =
  event "Halt"; 
  halt()
  
let rec bar x =
  event "Bar" ;
  if x>0 then bar (x-1)
  else x

let rec foo x =
  event "Foo";
  if x<=0 then foo x
  else halt()

let main () =
  let t = Random.int(0) in
     if Random.int(0)>0 then foo 0
     else foo(bar t)
(*
    ADAPTED: (Foo|Step)*
 *)


(* ORIGINAL: *)
(* Property to be checked: event Foo occurs infinitely often *)
(* The original property described in their paper is FG(Foo | Step):
   this can be expressed as the conjunction of the following properties:
    1. "Foo" happens infinitely often
    2. "Halt" happens only finitely often
    3. "Bar" happens only finitely often
 *)
(*{SPEC}
   fairness: (Foo, Never)
{SPEC}*)
