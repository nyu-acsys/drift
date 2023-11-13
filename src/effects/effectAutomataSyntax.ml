open Util
open Syntax

type pos = Syntax.pos
let fail = Syntax.fail

type state_t = Q of int
  
type assertCheck = PerTran | Final 

type aut_spec = {
  qset: state_t list; 
  env: var list;
  delta: term; 
  cfg0: term;
  asst: term option;
  asstFinal: term option
}

module type SemActionsSig = sig
  val delta_fn : term -> (term * (term list)) -> term -> (term * var list)
  val initial_cfg : (term * term) -> term
  val prop_assert : (term * (term list)) -> term -> (term * assertCheck)
  val prop_assert_final : (term * (term list)) -> term -> (term * assertCheck) 
end

module EvTrSemActions : SemActionsSig = struct

  let delta_fn x (q, acc) e =
    let pcfg = mk_fresh_var "cfg" in
    let eacc = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expecter")
      | [x] -> x
      | _ -> TupleLst (acc, "")
    in
    let term = mk_lambda x @@ 
                 mk_lambda pcfg (PatMat (pcfg, [ mk_pattern_case (TupleLst ([q; eacc], "")) e ], "")) 
    in (term, [])

  let initial_cfg (e1, e2) = TupleLst ([e1; e2], "")

  let prop_assert (q, acc) e = 
    let pcfg = mk_fresh_var "cfg" in
    let loc = {isast = true; ps = mk_default_loc} in (* TOOD: replace default_loc a relevant loc *)
    let eacc = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expecter")
      | [x] -> x
      | _ -> TupleLst (acc, "")
    in
    let asst = mk_lambda pcfg (PatMat (pcfg, [  
                                     mk_pattern_case 
                                       (TupleLst ([q; eacc], ""))
                                       (Ite (e, Const (UnitLit, ""), Const (UnitLit, ""), "", loc))
                                   ], ""))
    in (asst, PerTran)

  
  let prop_assert_final (q, acc) e = 
    let pcfg = mk_fresh_var "cfg" in
    let loc = {isast = true; ps = mk_default_loc} in (* TOOD: replace default_loc a relevant loc *)
    let eacc = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expecter")
      | [x] -> x
      | _ -> TupleLst (acc, "")
    in
    let asst = mk_lambda pcfg (PatMat (pcfg, [  
                                     mk_pattern_case 
                                       (TupleLst ([q; eacc], ""))
                                       (Ite (e, Const (UnitLit, ""), Const (UnitLit, ""), "", loc))
                                   ], ""))
    in (asst, Final)
end

module EvInterpSemActions : SemActionsSig = struct
 
  let rec add_vars xs env = match xs with 
    | [] -> env 
    | (Var (v, _))::vs -> v::(add_vars vs env)
    | (_)::vs -> add_vars vs env

  let delta_fn x (q, acc) e = (label e, add_vars acc [] |> add_vars [x])
  let initial_cfg (e1, e2) = TupleLst ([e1; e2], "")
  let prop_assert (q, acc) e = (e, PerTran)
  let prop_assert_final (q, acc) e = (e, Final)

end

let semActions = match (!Config.ev_trans) with
  | true -> (module EvTrSemActions: SemActionsSig)
  | false ->(module EvInterpSemActions: SemActionsSig) 
module SemActions = (val semActions : SemActionsSig)

let effect_aut_spec fields = 
  let qset, (delta, env), cfg0, assts = fields in
  let asst, asstFinal = List.fold_left (fun (a, af) (a', aCheck') -> 
                            match aCheck' with 
                            | PerTran -> (Some a', af)
                            | Final -> (a, Some a') ) (None, None) assts 
  in
  {qset; delta; env; cfg0; asst; asstFinal}

    
