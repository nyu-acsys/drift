open Kat
open AbstractDomain
open DriftSyntax
open Util
open SensitiveDomain
open SenSemantics


module SK = Symkat

(* Only track sequences of events (integers) *)

let kd_leq ke1 ke2 : bool = failwith "todo"
(*   match SK.compare ke1 ke2 with
   | (kl,kr),`L w -> true
   | _ -> failwith "leq not leq"*)

let kd_eq ke1 ke2 : bool = failwith "todo"
let kd_meet ke1 ke2 : Kat.expr' = 
    print_endline "needed meet_lst";
    Kat.one
let kd_join ke1 ke2 = Kat.pls ke1 ke2
let kd_widen ke1 ke2 : Kat.expr' = failwith "widen todo"

let kd_arrow ke var v : Kat.expr' = ke
let kd_stren ke : Kat.expr' = ke 
let kd_proj ke : Kat.expr' = ke (* don't care about env *)
let kd_rename ke = ke
let kd_replace ke = ke
let kd_reduce_len ke = failwith "todo"
let kd_list_cons ke v = 
  match v with 
  | Relation r -> failwith "todo"
   (* if the relation is a signle point, take that point. *)