(* Taken from 
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10 *)

let rec finish ():unit = event "Done";finish()
and reduce x r =
   if x<=0 then x else r x

let rec explore x r =
(*  event "Explore"; *)
  let y = reduce x r in
    if y<=0 then finish()
    else explore y r

let f x = x-2 

let main() =
  let t = Random.int(0) in
    explore t f

(* Property to be checked: event Done happens eventually in any infinite computation *)
(* The definition of "finish" above has been modified from the original one (of function "done")
   to turn "Done eventually happens" into "Done happens infinitely often" *)
(*{SPEC}
   fairness: (Done, Never)
{SPEC}*)
