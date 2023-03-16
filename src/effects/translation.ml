open DriftSyntax
open Util

type q = Q of int
type ev_val = int 
type 'a ev_ = ev_val -> q * a -> q * a
type acc_aut = { 
  qset: q list; 
  ev_: (ev_val list) ev_ 
}

let simpl_acc: acc_aut = {
  qset: [Q 0; Q 1]; 
  ev_ = fun (v: ev:val) ((q, vs): q * ev_val list) -> (q, v :: vs)
}

(* TODO 1: Define simpl_acc as an Drift AST : 
    TupleLst [
      TupleLst [Const _; ...] ;       // Q Set
      Rec                             // ev _ 
    ]
*) 

(* TODO 2: aut_cfg is a Drift AST : 
    TupleLst [
      Const _;                       // current q 
      Rec _                          // accumulator fun 
    ]
*)
let rec tr: acc_aut -> term -> aut_cfg -> term * aut_cfg = 
  fun a e ((q, acc) as acfg) ->
  match e with
  | TupleLst (e, l) -> TupleLst (List.map (fun e' -> tr a e' acfg) e, l)
  | (Const _ | Var _) -> (e, acfg)
  | App (e1, e2, l) -> 
    begin match tr a e1 acfg with
      | (Rec _ as e1', acfg1) ->
        let e2', acfg2 =  tr a e2 acfg1 in
        App (App (e1', acfg2, "?"), e2, l)
      | _ -> raise (Invalid_argument "Expected lambda abstraction")
    end
  | Rec _ -> (Rec (None, ((fresh_var "c"), ""), e, ""), acfg) 
  | UnOp (uop, e1, l) ->
    let e1', acfg1 = tr a e1 acfg in (UnOp (uop, e1', l), acfg1)
  | BinOp (bop, e1, e2, l) ->
    let e1', acfg1 = tr a e1 acfg in
    let e2', acfg2 = tr a e2 acfg1 in
    (BinOp (bop, e1', e2', l), acfg2)
  | Ite (b, et, ef, l, asst) ->
    let b', acfg1 = tr a b acfg in
    let et', acfg2 = tr a et afcg1 in
    let ef', acfg3 = tra a ef acfg1 in
    (Ite (b', et', ef', l, asst), if b' then acfg2 else acfg3)
  | Ev (e1, l) -> 
    let v, acfg1 = tr a e1 acfg in (Const (UnitLit), a.ev_ v acfg1)
  | PatMat (e1, patlist, l) ->
    let e1', acfg1 = tr a e1 acfg in
    let patlist' = List.map (fun (Case (p, e2)) -> Case (p, tr a t e2)) in
    (PatMat (e', patlist'), acfg1)

    
                                     
        
