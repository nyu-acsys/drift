open DriftSyntax
open EffectAutomataSyntax
open Util
open Printer

(* ============================================================================================ *)
(* ===                         I. Concrete Semantics AUTOMATON & Initial CFG                === *)
(* ============================================================================================ *)
let simple_aut_spec = "{ QSet   = [0];" ^ 
                      "  delta  = fun x (q1, acc1) -> (q1, x + acc1);" ^ 
                      "  assert = fun (q2, acc2) -> acc2 >= 6;" ^ 
                      "  IniCfg = (0, 0) }"

(* ============================================================================================ *)

(* ============================================================================================ *)
(* ===                         II. Reentrant-Lock AUTOMATON & Initial CFG                   === *)
(* ============================================================================================ *)
let reentrl_aut_spec = "{ QSet   = [0; 1];" ^ 
                       "  delta  = fun x (q, acc) -> if (q = 1) then (q, acc) else " ^ 
                       "     if (x + acc >= 0) then (q, x + acc) else (1, acc);" ^ 
                       "  assert = fun (q, acc) -> q <> 1 ;" ^ 
                       "  IniCfg = (0, 0) }"

(* ============================================================================================ *)
let parse_aut_spec s = 
  let lexbuf = Lexing.from_string s in
  EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf

let print_aut_spec (spec:aut_spec) = 
  print_endline "\nEffect Automaton Spec:";
  print_endline "\nQSet:";
  print_endline ("[" ^ (String.concat ";" (List.map (fun (Q q) -> string_of_int q) spec.qset)) ^ "]");
  print_endline "\ndelta:";
  print_exp stdout spec.delta;
  print_endline "\nassert:";
  print_exp stdout spec.asst;
  print_endline "\ninitial config:";
  print_exp stdout spec.cfg0;
  print_endline "\n<-------------------->\n"

(*
 * Translation any program e is given by:
 *   tr e simpl_ev simpl_cfg0   
 *)
let tr (e: term) (a: term) (acfg: term) (asst: term) = 
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
      let tr_e1 = tr_ e1 acfg in
      begin match tr_e1 with
        | TupleLst ([e1'; ecfg1], "") ->
          let tr_e2 = tr_ e2 ecfg1 in
          begin match tr_e2 with
            | TupleLst ([e2'; ecfg2], "") ->
              App ((mk_app e1' ecfg2), e2', l)
            | _ ->
              let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
              PatMat (tr_e2, [
                  mk_pattern_case 
                    (TupleLst ([e2x; acfg2x], ""))
                    (App ((mk_app e1' acfg2x), e2x, l))
                ], "")
          end
        | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_e1, [
              mk_pattern_case 
                (TupleLst ([e1x; acfg1x], ""))
                (PatMat (tr_ e2 acfg1x, [
                     mk_pattern_case 
                       (TupleLst ([e2x; acfg2x], ""))
                       (App ((mk_app e1x acfg2x), e2x, l))
                   ], ""))
            ], "")
      end
    | UnOp (uop, e1, l) ->
      let tr_e1 = tr_ e1 acfg in
      begin match tr_e1 with 
        | TupleLst ([e1'; ecfg1], "") ->
          ret (UnOp (uop, e1', l)) ecfg1
        | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_e1, [
              mk_pattern_case 
                (TupleLst ([e1x; acfg1x], ""))
                (ret (UnOp (uop, e1x, l)) acfg1x)
            ], "")
      end
    | BinOp (bop, e1, e2, l) -> 
      let tr_e1 = tr_ e1 acfg in
      begin match tr_e1 with
        | TupleLst ([e1'; ecfg1], "") ->
          let tr_e2 = tr_ e2 ecfg1 in
          begin match tr_e2 with
            | TupleLst ([e2'; ecfg2], "") ->
              ret (BinOp (bop, e1', e2', l)) ecfg2 
            | _ ->
              let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
              PatMat (tr_e2, [
                  mk_pattern_case
                    (TupleLst ([e2x; acfg2x], ""))
                    (ret (BinOp (bop, e1', e2x, l)) acfg2x)
                ], "")
          end
        | _ ->              
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_e1, [
              mk_pattern_case 
                (TupleLst ([e1x; acfg1x], ""))
                (PatMat (tr_ e2 acfg1x, [
                     mk_pattern_case 
                       (TupleLst ([e2x; acfg2x], ""))
                       (ret (BinOp (bop, e1x, e2x, l)) acfg2x)
                   ], ""))
            ], "")
      end
    | Ite (b, et, ef, l, asst) -> 
      let tr_b = tr_ b acfg in
      begin match tr_b with
        | TupleLst ([b'; ecfg1], "") ->
          Ite (b', tr_ et ecfg1, tr_ ef ecfg1, l, asst)
        | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_b, [
              mk_pattern_case 
                (TupleLst ([e1x; acfg1x], ""))
                (Ite (e1x, tr_ et acfg1x, tr_ ef acfg1x, l, asst))
            ], "")
      end
    | Event (e1, l) -> 
      ret (Const (UnitLit, "")) (mk_app (mk_app ev_ e1) acfg)
    | PatMat (e1, patlist, l) ->
      let tr_e1 = tr_ e1 acfg in
      begin match tr_e1 with
        | TupleLst ([e1'; ecfg1], "") ->
          PatMat (e1', (List.map (fun (Case (p, e2)) -> Case (p, tr_ e2 ecfg1)) patlist), l)
        | _ ->          
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_e1, [
              mk_pattern_case 
                (TupleLst ([e1x; acfg1x], ""))
                (PatMat (e1x, (List.map (fun (Case (p, e2)) -> Case (p, tr_ e2 acfg1x)) patlist), l)) 
            ], "")
      end
  in
  let e', acfg' = mk_fresh_var "e", mk_fresh_var "acfg" in
  let tr_e_asst = PatMat (tr_ e acfg, [
      mk_pattern_case
        (TupleLst ([e'; acfg'], ""))
        (mk_app asst acfg')
    ], "")
  in
  mk_app (mk_lambda ev_ tr_e_asst) a

let tr_simple e = 
  let aspec = parse_aut_spec reentrl_aut_spec in
  print_aut_spec (aspec);
  tr e aspec.delta aspec.cfg0 aspec.asst
