open Util
open DriftSyntax

type pos = DriftSyntax.pos
let fail = DriftSyntax.fail

type q = Q of int
  
type aut_spec = {
  qset: q list; 
  delta: term; 
  asst: term;
  cfg0: term
}

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

let initial_cfg = id

let effect_aut_spec fields = 
  let qset, delta, asst, cfg0 = fields in
  {qset; delta; asst; cfg0}

    
