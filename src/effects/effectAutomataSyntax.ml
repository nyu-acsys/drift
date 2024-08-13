open Util
open Syntax

type pos = Syntax.pos
let fail = Syntax.fail

type state_t = Q of int
  
type assertCheck = PerTran | Final 

type ml_aut_spec = {
  ml_qset: state_t list; 
  ml_env_vars: var list;
  ml_delta: mlterm; 
  ml_cfg0: mlterm;
  ml_asst: mlterm option;
  ml_asstFinal: mlterm option
}

type aut_spec = {
  qset: state_t list; 
  env_vars: var list;
  delta: term; 
  cfg0: term;
  asst: term option;
  asstFinal: term option
}

let aut_spec_of_ml (spec: ml_aut_spec) (label_term: bool) =
  let term_of_ml_and_label = match label_term with true -> compose label term_of_ml | false -> term_of_ml in
  {
    qset = spec.ml_qset; 
    env_vars = spec.ml_env_vars; 
    delta = term_of_ml_and_label spec.ml_delta; 
    cfg0 = term_of_ml spec.ml_cfg0; 
    asst = Option.map term_of_ml_and_label spec.ml_asst; 
    asstFinal = Option.map term_of_ml_and_label spec.ml_asstFinal
  }  

module SSA = Syntax.SemActions

module type SemActionsSig = sig
  val delta_fn : mlterm -> (mlterm * (mlterm list)) -> mlterm -> (mlterm * var list)
  val initial_cfg : (mlterm * mlterm) -> mlterm
  val prop_assert : (mlterm * (mlterm list)) -> mlterm -> (mlterm * assertCheck)
  val prop_assert_final : (mlterm * (mlterm list)) -> mlterm -> (mlterm * assertCheck) 
end

module EvMlTrSemActions : SemActionsSig = struct 
  let fresh_var = compose (fun x -> MlVar x) fresh_var  
  
  let delta_fn x (q, acc) e = 
    let pcfg = fresh_var "cfg" in
    let delta_term = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expected")
      | [p] -> MlRec (None, [x; pcfg], MlPatMat (pcfg, [MlCase (MlTupleLst [q; p], e)]))
      | ps ->
         let pacc = fresh_var "acc" in
         MlRec (None, [x; pcfg], 
                MlPatMat (pcfg, [
                      MlCase (MlTupleLst [q; pacc], 
                              MlPatMat (pacc, [MlCase (MlTupleLst ps, e)]))]))
    in 
    (delta_term, [])

  let initial_cfg (e1, e2) = MlTupleLst [e1; e2]

  let mk_assert (q, acc) e = 
    let pcfg = fresh_var "cfg" in
    match acc with
    | [] -> raise (Invalid_argument "Accumulator expression expected")
    | [p] -> 
       MlRec (None, [pcfg], MlPatMat (pcfg, [MlCase (MlTupleLst [q; p], MlAssert (e, mk_default_loc))]))
    | ps ->
       let pacc = fresh_var "acc" in
       MlRec (None, [pcfg], MlPatMat (pcfg, [
                                  MlCase (MlTupleLst [q; pacc], 
                                          MlPatMat (pacc, [
                                                MlCase (MlTupleLst ps, 
                                                        MlAssert(e, mk_default_loc))]))]))

  let prop_assert (q, acc) e = (mk_assert (q, acc) e, PerTran)

  let prop_assert_final (q, acc) e = (mk_assert (q, acc) e, Final)
end

module EvInterpSemActions : SemActionsSig = struct
 
  let rec add_vars xs res = match xs with 
    | [] -> res
    | (MlVar v)::vs -> v::(add_vars vs res)
    | (_)::vs -> add_vars vs res

  let delta_fn x (q, acc) e = (e, add_vars acc [] |> add_vars [x])
  let initial_cfg (e1, e2) = MlTupleLst [e1; e2]
  let prop_assert (q, acc) e = (e, PerTran)
  let prop_assert_final (q, acc) e = (e, Final)

end

let semActions = match (!Config.ev_trans) with
  | true -> (module EvMlTrSemActions: SemActionsSig)
  | false ->(module EvInterpSemActions: SemActionsSig) 
module SemActions = (val semActions : SemActionsSig)

let effect_aut_spec fields = 
  let ml_qset, (ml_delta, ml_env_vars), ml_cfg0, ml_assts = fields in
  let ml_asst, ml_asstFinal = List.fold_left (fun (a, af) (a', aCheck') -> 
                            match aCheck' with 
                            | PerTran -> (Some a', af)
                            | Final -> (a, Some a') ) (None, None) ml_assts 
  in
  {ml_qset; ml_delta; ml_env_vars; ml_cfg0; ml_asst; ml_asstFinal}


