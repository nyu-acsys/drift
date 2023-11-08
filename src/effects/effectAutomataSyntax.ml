open Util
open Syntax

type pos = Syntax.pos
let fail = Syntax.fail

type state_t = Q of int
  
type aut_spec = {
  qset: state_t list; 
  delta: term; 
  asst: term;
  cfg0: term
}

module type SemActionsSig = sig
  val delta_fn : term -> (term * term) -> term -> term
  val effect_assert : (term * term) -> term -> term
  val initial_cfg : (term * term) -> term
end

module EvTrSemActions : SemActionsSig = struct

  let delta_fn x (q, acc) e =
    let pcfg = mk_fresh_var "cfg" in
    mk_lambda x @@ 
      mk_lambda pcfg (PatMat (pcfg, [ mk_pattern_case (TupleLst ([q; acc], "")) e ], "")) 

  let effect_assert (q, acc) e = 
    let pcfg = mk_fresh_var "cfg" in
    let loc = {isast = true; ps = mk_default_loc} in (* TOOD: replace default_loc a relevant loc *)
    mk_lambda pcfg (PatMat (pcfg, [  
                          mk_pattern_case 
                            (TupleLst ([q; acc], ""))
                            (Ite (e, Const (UnitLit, ""), Const (UnitLit, ""), "", loc))
                        ], ""))

  let initial_cfg (e1, e2) = TupleLst ([e1; e2], "")

end

module EvInterpSemActions : SemActionsSig = struct
 
  let delta_fn x (q, acc) e = label e
  let effect_assert (q, acc) e = e
  let initial_cfg (e1, e2) = TupleLst ([e1; e2], "")

end

let semActions = match (!Config.ev_trans) with
  | true -> (module EvTrSemActions: SemActionsSig)
  | false ->(module EvInterpSemActions: SemActionsSig) 
module SemActions = (val semActions : SemActionsSig)

let effect_aut_spec fields = 
  let qset, delta, asst, cfg0 = fields in
  {qset; delta; asst; cfg0}

    
