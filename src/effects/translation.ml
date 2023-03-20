open DriftSyntax
open Util

(* simpl_acc_t = \x.\cfg.match cfg with (q, acc) -> (q, x::acc)  *)
let simpl_acc_t: term =
  let px = mk_var "x" in
  let pcfg = mk_var "x" in
  let q_pat = mk_var "x" in
  let acc_pat = mk_var "x" in
  mk_lambda px @@ mk_lambda pcfg (PatMat (pcfg, 
                                          [(mk_pattern_case 
                                             (TupleLst ([q_pat; acc_pat], ""))            
                                             (TupleLst ([q_pat; mk_op Cons px acc_pat], "")))],
                                          ""))

(* cfg0: (0, []) describes the initial configuration where 
 *    q0 is the initial state of the automaton 
 *    [] is the empty accumulator 
 *)
let simpl_cfg0: term = TupleLst ([Const (Integer 0, ""); Const (IntList [], "")], "") 

(*
 * Translation any program p is given by:
 *   tr_outer p simpl_acc_t cfg0_t   
 *)
let tr (e: term) (a: term) (acfg: term) = 
  let ev_ = mk_var "ev_" in
  let ret e cfg = TupleLst ([e; cfg], "") in
  let rec tr_ (e: term) (acfg: term) = 
    match e with
    | TupleLst (e, l) -> TupleLst (List.map (fun e' -> tr_ e' acfg) e, l)
    | (Const _ | Var _) -> ret e acfg
    | App (e1, e2, l) -> 
      begin match tr_ e1 acfg with
        | TupleLst ([Rec _ as e1'; acfg1], _) -> begin match tr_ e2 acfg1 with 
            | TupleLst ([e2'; acfg2], _) -> App ((mk_app e1' acfg2), e2, l)
            | _ -> raise (Invalid_argument "Expected a pair")
          end
        | _ -> raise (Invalid_argument "Expected a lambda abstr_action")
      end
    | Rec (fopt, px, def, l) ->
      let fc = fresh_var "c" in
      ret (Rec (fopt, (fc, ""), Rec (None, px, tr_ def (Var (fc, "")), l), "")) acfg
    | UnOp (uop, e1, l) -> begin match tr_ e1 acfg with
        | TupleLst ([e1'; acfg1], _) -> ret (UnOp (uop, e1', l)) acfg1
        | _ -> raise (Invalid_argument "Expected a pair")
      end
    | BinOp (bop, e1, e2, l) -> begin match tr_ e1 acfg with  
        | TupleLst ([e1'; acfg1], _) -> begin match tr_ e2 acfg1 with 
            | TupleLst ([e2'; acfg2], _) -> ret (BinOp (bop, e1', e2', l)) acfg2
            | _ -> raise (Invalid_argument "Expected a pair")
          end
        | _ -> raise (Invalid_argument "Expected a pair")
      end
    | Ite (b, et, ef, l, asst) -> begin match tr_ b acfg with
        | TupleLst ([b'; acfg1], _) -> Ite (b', tr_ et acfg1, tr_ ef acfg1, l, asst)
        | _ -> raise (Invalid_argument "Expected a pair")
      end
    | Event (e1, l) -> ret (Const (UnitLit, "")) (mk_app (mk_app ev_ e1) acfg)
    | PatMat (e1, patlist, l) -> begin match tr_ e1 acfg with
        | TupleLst ([e1'; acfg1], _) -> 
          let patlist' = List.map (fun (Case (p, e2)) -> Case (p, tr_ e2 acfg1)) patlist in
          ret (PatMat (e1', patlist', l)) acfg1
        | _ -> raise (Invalid_argument "Expected a pair")
      end
  in
  mk_app (mk_lambda ev_ (tr_ e acfg)) a 

    
                                     
        
