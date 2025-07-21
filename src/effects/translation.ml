open Syntax
open EffectAutomataSyntax
open Printer

(*
 * Translation any program e is given by:
 *   tr e simpl_ev simpl_cfg0   
 *)

let tr (e: term) (a: term) (acfg: term) (asst: term option) (asstFinal: term option) = 
  let ev_ = mk_fresh_var "ev_" in
  let ret e cfg = TupleLst ([e; cfg], "") in     
  let rec tr_tuple_ (es: term list) (escfg : term) (acc : term list) = 
    match es with
    | [] -> TupleLst ([TupleLst (List.rev acc, ""); escfg], "")
    | e'::es' -> begin
       let tr_e' = tr_ e' escfg in
       begin match tr_e' with
       | TupleLst ([e''; ecfg''], "") -> tr_tuple_ es' ecfg'' (e'' :: acc)
       | _ ->
          let ex'', ecfgx'' = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_e', [
                            mk_pattern_case 
                              (TupleLst ([ex''; ecfgx''], ""))
                              (tr_tuple_ es' ecfgx'' (ex'' :: acc))
                          ], "")
       end
      end
  and tr_ (e: term) (acfg: term) = 
    match e with
    | TupleLst (e, _) -> tr_tuple_ e acfg []
    | (Const _ | Var _ | NonDet _) -> ret e acfg
    | Rec (fopt, px, def, l) ->
      let fc = fresh_var "cfg" in
      ret (Rec (fopt, px, Rec (None, (fc, ""), tr_ def (Var (fc, "")), l), "")) acfg
    | App (e1, e2, l) ->
      let tr_e1 = tr_ e1 acfg in
      begin match tr_e1 with
        | TupleLst ([e1'; ecfg1], "") ->
          let tr_e2 = tr_ e2 ecfg1 in
          begin match tr_e2 with
            | TupleLst ([e2'; ecfg2], "") ->
              App ((mk_app e1' e2'), ecfg2, l)
            | _ ->
              let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
              PatMat (tr_e2, [
                  mk_pattern_case 
                    (TupleLst ([e2x; acfg2x], ""))
                    (App ((mk_app e1' e2x), acfg2x, l))
                ], "")
          end
        | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          let tr_e2 = tr_ e2 acfg1x in
          begin match tr_e2 with
            | TupleLst ([e2'; ecfg2], "") -> 
              PatMat (tr_e1, [
                  mk_pattern_case
                    (TupleLst ([e1x; acfg1x], ""))
                    (App ((mk_app e1x e2'), ecfg2, l))
                    ], "")
            | _ ->              
              let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
              PatMat (tr_e1, [
                  mk_pattern_case 
                    (TupleLst ([e1x; acfg1x], ""))
                    (PatMat (tr_e2, [
                         mk_pattern_case 
                           (TupleLst ([e2x; acfg2x], ""))
                           (App ((mk_app e1x e2x), acfg2x, l))
                       ], ""))
                ], "")
          end
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
    | Ite (b, et, ef, l) -> 
      let tr_b = tr_ b acfg in
      begin match tr_b with
        | TupleLst ([b'; ecfg1], "") ->
          Ite (b', tr_ et ecfg1, tr_ ef ecfg1, l)
        | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          PatMat (tr_b, [
              mk_pattern_case 
                (TupleLst ([e1x; acfg1x], ""))
                (Ite (e1x, tr_ et acfg1x, tr_ ef acfg1x, l))
            ], "")
      end
    | Event (e1, _) -> 
       let tr_e1 = tr_ e1 acfg in
       begin match tr_e1, asst with
         | TupleLst ([e1'; e1cfg], ""), None -> 
            ret (Const (UnitLit, "")) (mk_app (mk_app ev_ e1') e1cfg)
         | TupleLst ([e1'; e1cfg], ""), Some asst ->
            let acfgx = mk_fresh_var "cfg" in
            mk_app 
              (mk_lambda acfgx (mk_op Seq (mk_app asst acfgx) (ret (Const (UnitLit, "")) acfgx)))
            (mk_app (mk_app ev_ e1') e1cfg)
             
         | _, None -> 
            let e1x, e1cfg = mk_fresh_var "x", mk_fresh_var "cfg" in
            PatMat (tr_e1, [
                  mk_pattern_case 
                    (TupleLst ([e1x; e1cfg], ""))
                    (ret (Const (UnitLit, "")) (mk_app (mk_app ev_ e1x) e1cfg))
                ], "")
         | _, Some asst -> 
            let acfgx = mk_fresh_var "cfg" in
            let e1x, e1cfg = mk_fresh_var "x", mk_fresh_var "cfg" in
            PatMat (tr_e1, [
                  mk_pattern_case 
                    (TupleLst ([e1x; e1cfg], ""))
                    (mk_app 
                       (mk_lambda acfgx 
                          (mk_op Seq (mk_app asst acfgx) (ret (Const (UnitLit, "")) acfgx)))
                       (mk_app (mk_app ev_ e1x) e1cfg))
                ], "")
             
        end
    | Assert (_, _, _) -> 
       ret e acfg
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
    | _ -> failwith "Invalid expression"
  in
  let tr_e = match asstFinal with
    | None -> tr_ e acfg
    | Some asst -> 
       let e', acfg' = mk_fresh_var "e", mk_fresh_var "acfg" in
       PatMat (tr_ e acfg, [
             mk_pattern_case
               (TupleLst ([e'; acfg'], ""))
               (BinOp(Seq, (mk_app asst acfg'), e', ""))
           ], "")
  in
  mk_app (mk_lambda ev_ tr_e) a

let parse_aut_spec file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let a =  aut_spec_of_ml (EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf) false in
  let _ = close_in chan in
  a

let print_aut_spec (spec:aut_spec) = 
  print_endline "\nProperty Automaton Spec:";
  print_endline "\nQSet:";
  print_endline ("[" ^ (String.concat ";" (List.map (fun (Q q) -> string_of_int q) spec.qset)) ^ "]");
  print_endline "\ndelta:";
  print_exp stdout spec.delta;
  print_endline "\nassert:";
  (match spec.asst with Some asst -> print_exp stdout asst | None -> Format.printf "None");
  print_endline "\nassertFinal:";
  (match spec.asstFinal with Some asst -> print_exp stdout asst | None -> Format.printf "None");
  print_endline "\ninitial config:";
  print_exp stdout spec.cfg0;
  print_endline "\n<-------------------->\n"

let tr_effect spec e = 
  let aspec = parse_aut_spec spec in 
  (if !Config.out_put_level < 2 then print_aut_spec (aspec));
  tr e aspec.delta aspec.cfg0 aspec.asst aspec.asstFinal
