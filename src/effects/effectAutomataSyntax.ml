open Util
open Syntax

type pos = Syntax.pos
let fail = Syntax.fail

type state_t = Q of int
  
type assertCheck = PerTran | Final 

type aut_spec = {
  qset: state_t list; 
  env_vars: var list;
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
    let delta_term = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expected")
      | [p] -> mk_lambda x @@ 
                mk_lambda pcfg 
                  (PatMat (pcfg, [ mk_pattern_case (TupleLst ([q; p], "")) e ], ""))
      | ps -> 
         begin
           let pacc = mk_fresh_var "acc" in
           mk_lambda x @@
             mk_lambda pcfg
               (PatMat 
                  (pcfg, 
                   [ mk_pattern_case 
                       (TupleLst ([q; pacc], ""))
                       (PatMat 
                          (pacc, 
                           [ mk_pattern_case (TupleLst (ps, "")) e ], 
                           ""))], ""))
         end
    in 
    (delta_term, [])

  let initial_cfg (e1, e2) = TupleLst ([e1; e2], "")

  let prop_assert (q, acc) e = 
    let pcfg = mk_fresh_var "cfg" in
    let asst_term = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expected")
      | [p] ->
         mk_lambda pcfg 
           (PatMat 
              (pcfg, 
               [ mk_pattern_case 
                   (TupleLst ([q; p], ""))
                   (Assert (e, mk_default_loc, ""))
               ], ""))
      | ps -> 
         begin
           let pacc = mk_fresh_var "acc" in
           mk_lambda pcfg 
             (PatMat 
                (pcfg, 
                 [ mk_pattern_case 
                     (TupleLst ([q; pacc], ""))
                     (PatMat 
                        (pacc,
                         [ mk_pattern_case
                           (TupleLst (ps, ""))
                           (Assert (e, mk_default_loc, "")) ], ""))
                 ], ""))
         end
    in
    (asst_term, PerTran)

  
  let prop_assert_final (q, acc) e = 
    let pcfg = mk_fresh_var "cfg" in
    let asst_term = match acc with
      | [] -> raise (Invalid_argument "Accumulator expression expected")
      | [p] ->
         mk_lambda pcfg 
           (PatMat 
              (pcfg, 
               [ mk_pattern_case 
                   (TupleLst ([q; p], ""))
                   (Assert (e, mk_default_loc, ""))
               ], ""))
      | ps -> 
         begin
           let pacc = mk_fresh_var "acc" in
           mk_lambda pcfg 
             (PatMat 
                (pcfg, 
                 [ mk_pattern_case 
                     (TupleLst ([q; pacc], ""))
                     (PatMat 
                        (pacc,
                         [ mk_pattern_case
                           (TupleLst (ps, ""))
                           (Assert (e, mk_default_loc, "")) ], ""))
                 ], ""))
         end
    in
    (asst_term, Final)
end

module EvInterpSemActions : SemActionsSig = struct
 
  let rec add_vars xs res = match xs with 
    | [] -> res
    | (Var (v, _))::vs -> v::(add_vars vs res)
    | (_)::vs -> add_vars vs res

  let delta_fn x (q, acc) e = (label e, add_vars acc [] |> add_vars [x])
  let initial_cfg (e1, e2) = TupleLst ([e1; e2], "")
  let prop_assert (q, acc) e = (label e, PerTran)
  let prop_assert_final (q, acc) e = (label e, Final)

end

let semActions = match (!Config.ev_trans) with
  | true -> (module EvTrSemActions: SemActionsSig)
  | false ->(module EvInterpSemActions: SemActionsSig) 
module SemActions = (val semActions : SemActionsSig)

let effect_aut_spec fields = 
  let qset, (delta, env_vars), cfg0, assts = fields in
  let asst, asstFinal = List.fold_left (fun (a, af) (a', aCheck') -> 
                            match aCheck' with 
                            | PerTran -> (Some a', af)
                            | Final -> (a, Some a') ) (None, None) assts 
  in
  {qset; delta; env_vars; cfg0; asst; asstFinal}

    
