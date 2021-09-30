(* Taken from 
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10 *)

let rec finish ():unit = event "Done";finish()
and reduce x r =
   if x<=0 then x else r x

let rec explore x r =
  event "Explore";
  let y = reduce x r in
    if y<=0 then finish()
    else explore y r

let f x = x-2 

let main() =
  let t = Random.int(0) in
    explore t f

(*
   ADAPTED: Explore* . Done*
   Abstraction: q0 --explore--> q1 --done--> q2
      q0 Explore q1
      q1 Explore q1
      q1 Done q2
      q2 Done q2

   ADT: list tracks events seen so far
     refinement type based on automata

     events : { {Explore,Done} list | q0 --explore--> q1 --done--> q2 }

      Explore; E; E; E) • E
      Explore* • Explore = Explore*

      *)


(* ORIGINAL: *)
(* Property to be checked: event Done happens eventually in any infinite computation *)
(* The definition of "finish" above has been modified from the original one (of function "done")
   to turn "Done eventually happens" into "Done happens infinitely often" *)
(*{SPEC}
   fairness: (Done, Never)
{SPEC}*)
