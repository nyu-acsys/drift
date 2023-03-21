open DriftSyntax
open Util
open Printer

(* ============================================================================================ *)
(* ===                         I. Concrete Semantics AUTOMATON & Initial CFG                === *)
(* ============================================================================================ *)
(* simpl_ev_ = \x.\cfg.match cfg with (q, acc) -> (q, x::acc)  *)
let simpl_ev_: term =
  let px = mk_fresh_var "x" in
  let pcfg = mk_fresh_var "cfg" in
  let q_pat = mk_fresh_var "q" in
  let acc_pat = mk_fresh_var "acc" in
  mk_lambda px @@ mk_lambda pcfg (PatMat (pcfg, [
      mk_pattern_case 
        (TupleLst ([q_pat; acc_pat], ""))            
        (TupleLst ([q_pat; mk_op Cons px acc_pat], ""))
    ], ""))

(* simpl_cfg0: (0, []) describes the initial configuration where 
 *    q0 is the initial state of the automaton 
 *    [] is the empty accumulator 
 *)
let simpl_cfg0: term = TupleLst ([Const (Integer 0, ""); Const (IntList [], "")], "") 
(* ============================================================================================ *)

(* ============================================================================================ *)
(* ===                         II. Reentrant-Lock AUTOMATON & Initial CFG                   === *)
(* ============================================================================================ *)
(* reentrl_ev_ = \x.\cfg.match cfg with 
 *                        (q, acc) -> if (q = q_err) 
 *                                    then (q_err, acc) 
 *                                    else (if (x + acc >= 0) 
 *                                          then (q, x + acc)
 *                                          else (q_err, acc)) *)
let reentrl_ev_: term = 
  let px = mk_fresh_var "x" in
  let pcfg = mk_fresh_var "cfg" in
  let q_pat = mk_fresh_var "q" in
  let acc_pat = mk_fresh_var "acc" in
  let q_err = mk_int (-1) in
  mk_lambda px @@ mk_lambda pcfg (PatMat (pcfg, [
      mk_pattern_case 
        (TupleLst ([q_pat; acc_pat], ""))            
        ( mk_ite 
            (mk_op Eq q_pat q_err)
            (TupleLst ([q_pat; acc_pat], ""))
            (mk_ite 
               (mk_op Ge (mk_op Plus px acc_pat) (mk_int 0))
               (TupleLst ([q_pat; mk_op Plus px acc_pat], ""))
               (TupleLst ([q_err; acc_pat], ""))
            )
        )
    ], ""))
(* reentrl_cfg0: (0, []) describes the initial configuration where 
 *    q0 is the initial state of the automaton 
 *    0 is the base value 
 *)
let reentrl_cfg0: term = TupleLst ([Const (Integer 0, ""); Const (Integer 0, "")], "") 
(* ============================================================================================ *)

(*
 * Translation any program e is given by:
 *   tr e simpl_ev simpl_cfg0   
 *)
let tr (e: term) (a: term) (acfg: term) = 
  let ev_ = mk_fresh_var "ev_" in
  let ret e cfg = TupleLst ([e; cfg], "") in
  let rec tr_ (e: term) (acfg: term) = 
    match e with
    | TupleLst (e, l) -> TupleLst (List.map (fun e' -> tr_ e' acfg) e, l)
    | (Const _ | Var _) -> ret e acfg
    | Rec (fopt, px, def, l) ->
      let fc = fresh_var "cfg" in
      ret (Rec (fopt, (fc, ""), Rec (None, px, tr_ def (Var (fc, "")), l), "")) acfg
    | App (e1, e2, l) -> 
      let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
      let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
      PatMat (tr_ e1 acfg, [
          mk_pattern_case 
            (TupleLst ([e1x; acfg1x], ""))
            (PatMat (tr_ e2 acfg1x, [
                 mk_pattern_case 
                   (TupleLst ([e2x; acfg2x], ""))
                   (App ((mk_app e1x acfg2x), e2x, l))
               ], ""))
        ], "")
    | UnOp (uop, e1, l) ->
      let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
      PatMat (tr_ e1 acfg, [
          mk_pattern_case 
            (TupleLst ([e1x; acfg1x], ""))
            (ret (UnOp (uop, e1x, l)) acfg1x)
        ], "")
    | BinOp (bop, e1, e2, l) -> 
      let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
      let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
      PatMat (tr_ e1 acfg, [
          mk_pattern_case 
            (TupleLst ([e1x; acfg1x], ""))
            (PatMat (tr_ e2 acfg1x, [
                 mk_pattern_case 
                   (TupleLst ([e2x; acfg2x], ""))
                   (ret (BinOp (bop, e1x, e2x, l)) acfg2x)
               ], ""))
        ], "")
    | Ite (b, et, ef, l, asst) -> 
      let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
      PatMat (tr_ b acfg, [
          mk_pattern_case 
            (TupleLst ([e1x; acfg1x], ""))
            (Ite (e1x, tr_ et acfg1x, tr_ ef acfg1x, l, asst))
        ], "")
    | Event (e1, l) -> (print_endline "\nEvent"; print_exp stdout e); 
      ret (Const (UnitLit, "")) (mk_app (mk_app ev_ e1) acfg)
    | PatMat (e1, patlist, l) ->
      let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
      PatMat (tr_ e1 acfg, [
          mk_pattern_case 
            (TupleLst ([e1x; acfg1x], ""))
            (PatMat (e1x, (List.map (fun (Case (p, e2)) -> Case (p, tr_ e2 acfg1x)) patlist), l)) 
        ], "")
  in
  mk_app (mk_lambda ev_ (tr_ e acfg)) a 
