open Util
open Syntax
open EffectAutomataSyntax
open Printer

(* translate of an OCaml program into a tuple encoding *)
(* - args: the parsed program (= a list of lambda-abstractions definitions. *)
(* --- *)
(* alg. steps: 
   - add the ev_step function; include here the ev assertion
   - translate the expressions
   - identify the main function and use a let binding for the body to capture the result
   - insert the final assertion
*)
(* --- *)
(* output: 
   - translate the expression
*)

let property_spec: (string * ml_aut_spec) option ref = ref None

let parse_aut_spec file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let a = EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf in 
  let a_raw = (seek_in chan 0; really_input_string chan (in_channel_length chan)) in
  let _ = close_in chan in
  (a_raw, a)

let parse_property_spec prop_file = 
  let spec = parse_aut_spec prop_file in
  spec

let tr: mlterm -> mlterm -> mlterm -> mlterm option -> mlterm option -> mlterm = 
  fun e ev_step acfg asst asstFinal ->
  let mk_fresh_var = compose (fun x -> MlVar x) fresh_var in
  let ret e cfg = MlTupleLst [e; cfg] in
  let rec tr_ (e: mlterm) (acfg: mlterm) = 
    match e with
    | MlTupleLst e -> MlTupleLst (List.map (fun e' -> tr_ e' acfg) e)
    | (MlConst _ | MlVar _ | MlNonDet) -> ret e acfg
    | MlRec (fopt, xs, def) ->
       let fc = mk_fresh_var "cfg" in
       begin match xs with
       | [] -> raise (Invalid_argument "params missing")
       | [x] -> ret (MlRec (fopt, [x; fc], tr_ def fc)) acfg
       | x::xs' -> ret (MlRec (fopt, [x; fc], tr_ (MlRec (None, xs', def)) fc)) acfg
       end
    | MlApp (e1, e2) ->
       let tr_e1 = tr_ e1 acfg in
       begin match tr_e1 with
        | MlTupleLst [e1'; ecfg1] ->
           let tr_e2 = tr_ e2 ecfg1 in
           begin match tr_e2 with
           | MlTupleLst [e2'; ecfg2] ->
              MlApp (MlApp (e1', e2'), ecfg2)
           | _ ->  
              let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
              MlPatMat (tr_e2, [
                    MlCase (MlTupleLst [e2x; acfg2x], MlApp (MlApp (e1', e2x), acfg2x))])
           end
        | _ ->
           let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
           let tr_e2 = tr_ e2 acfg1x in
           begin match tr_e2 with
           | MlTupleLst [e2'; ecfg2] -> 
              MlPatMat (tr_e1, [
                    MlCase (MlTupleLst [e1x; acfg1x], MlApp (MlApp (e1x, e2'), ecfg2))])
            | _ ->              
               let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
               MlPatMat (tr_e1, [
                     MlCase (MlTupleLst [e1x; acfg1x],
                             MlPatMat (tr_e2, [
                                   MlCase (MlTupleLst [e2x; acfg2x],
                                           MlApp (MlApp (e1x, e2x), acfg2x))]))])
           end
       end 
    | MlUnOp (uop, e1) ->
       let tr_e1 = tr_ e1 acfg in
       begin match tr_e1 with 
       | MlTupleLst [e1'; ecfg1] ->
          ret (MlUnOp (uop, e1')) ecfg1
       | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          MlPatMat (tr_e1, [
                MlCase (MlTupleLst [e1x; acfg1x], ret (MlUnOp (uop, e1x)) acfg1x)])
       end
    | MlBinOp (bop, e1, e2) -> 
      let tr_e1 = tr_ e1 acfg in
      begin match tr_e1 with
      | MlTupleLst [e1'; ecfg1] ->
         let tr_e2 = tr_ e2 ecfg1 in
         begin match tr_e2 with
         | MlTupleLst [e2'; ecfg2] ->
            ret (MlBinOp (bop, e1', e2')) ecfg2 
         | _ ->
            let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
            MlPatMat (tr_e2, [
                  MlCase (MlTupleLst [e2x; acfg2x], ret (MlBinOp (bop, e1', e2x)) acfg2x)])
         end
      | _ ->              
         let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
         let e2x, acfg2x = mk_fresh_var "x", mk_fresh_var "cfg" in
         MlPatMat (tr_e1, [
               MlCase (MlTupleLst [e1x; acfg1x],
                       MlPatMat (tr_ e2 acfg1x, [
                             MlCase (MlTupleLst [e2x; acfg2x], 
                                     ret (MlBinOp (bop, e1x, e2x)) acfg2x)]))])
      end
    | MlIte (b, et, ef) -> 
       let tr_b = tr_ b acfg in
       begin match tr_b with
       | MlTupleLst [b'; ecfg1] ->
          MlIte (b', tr_ et ecfg1, tr_ ef ecfg1)
       | _ ->
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          MlPatMat (tr_b, [
                MlCase (MlTupleLst [e1x; acfg1x], MlIte (e1x, tr_ et acfg1x, tr_ ef acfg1x))])
       end
    | MlEvent e1 -> 
       let tr_e1 = tr_ e1 acfg in
       begin match tr_e1, asst with
       | MlTupleLst [e1'; e1cfg], None -> 
          ret (MlConst UnitLit) (MlApp (MlApp (ev_step, e1'), e1cfg))
       | MlTupleLst [e1'; e1cfg], Some asst ->
          let acfgx = mk_fresh_var "cfg" in
          MlApp (MlRec (None, [acfgx], 
                        MlBinOp (Seq, MlApp (asst, acfgx), ret (MlConst UnitLit) acfgx)),
                 MlApp (MlApp (ev_step, e1'), e1cfg))
       | _, None -> 
          let e1x, e1cfg = mk_fresh_var "x", mk_fresh_var "cfg" in
          MlPatMat (tr_e1, [
                MlCase (MlTupleLst [e1x; e1cfg],
                        ret (MlConst UnitLit) (MlApp (MlApp (ev_step, e1x), e1cfg)))])
       | _, Some asst -> 
          let acfgx = mk_fresh_var "cfg" in
          let e1x, e1cfg = mk_fresh_var "x", mk_fresh_var "cfg" in
          MlPatMat (tr_e1, [
                MlCase (MlTupleLst [e1x; e1cfg],
                        MlApp (MlRec (None, [acfgx],  
                                      MlBinOp (Seq, (MlApp (asst, acfgx)), 
                                               (ret (MlConst UnitLit) acfgx))),
                               MlApp (MlApp (ev_step, e1x), e1cfg)))])
       end
    | MlAssert (_, _) -> 
       ret e acfg
    | MlPatMat (e1, patlist) ->
       let tr_e1 = tr_ e1 acfg in
       begin match tr_e1 with
       | MlTupleLst [e1'; ecfg1] ->
          MlPatMat (e1', (List.map (fun (MlCase (p, e2)) -> MlCase (p, tr_ e2 ecfg1)) patlist))
       | _ ->          
          let e1x, acfg1x = mk_fresh_var "x", mk_fresh_var "cfg" in
          MlPatMat (tr_e1, [
                MlCase (MlTupleLst [e1x; acfg1x],
                      MlPatMat (e1x, (List.map (fun (MlCase (p, e2)) -> 
                                          MlCase (p, tr_ e2 acfg1x)) patlist)))])
       end
    | MlLetIn (name, e1, e2) ->
       let name' = fresh_var name in
       let acfg1n = mk_fresh_var "cfg" in
       MlLetIn (name', tr_ e1 acfg, 
                MlPatMat (MlVar name', [
                      MlCase (MlTupleLst [(MlVar name); acfg1n], tr_ e2 acfg1n)]))
    | (MlGDefs _ | MlGDefMain _) -> raise (Invalid_argument "Not supported")
  in
  let tr_e = match asstFinal with
    | None -> tr_ e acfg
    | Some asst -> 
       let e', acfg' = mk_fresh_var "e", mk_fresh_var "acfg" in
       MlPatMat (tr_ e acfg, [
             MlCase (MlTupleLst [e'; acfg'],
                     MlBinOp (Seq, MlApp (asst, acfg'), e'))])
  in
  tr_e

let parse_aut_spec file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let a =  EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf in
  let a_raw = (seek_in chan 0; really_input_string chan (in_channel_length chan)) in
  let _ = close_in chan in
  (a_raw, a)

let parse_property_spec prop_file = 
  let spec = parse_aut_spec prop_file in
  spec

let run (e: mlterm) = 
  let ev_step = fresh_var "ev_step" in 
  let asst = fresh_var "ev_step_asst" in
  let asstFinal = fresh_var "asst_final" in
  (* let _ = match !property_spec with *)
  (*   | None -> Format.printf "No property file\n" *)
  (*   | Some (_, spec) -> Format.printf "Some spec\n" *)
  (* in *)
  (* let _ = match e with *)
  (*   | MlGDefs (ges, Some (MlGDefMain (MlRec  _)))  -> Format.printf "ml gdefs\n" *)
  (*   | _ -> Format.printf "ml other\n" *)
  (* in *)
  match e, !property_spec  with
  | MlGDefs (ges, Some (MlGDefMain (MlRec (fopt, xs, main_body)))), Some (_, spec) ->
     let gasst = Option.map (fun e -> [(asst, e)]) spec.ml_asst 
                 |> Option.value ~default:[] in
     let gasstF = Option.map (fun e -> [(asstFinal, e)]) spec.ml_asstFinal 
                  |> Option.value ~default:[] in
     let vev_step = MlVar ev_step in
     let vasst = Option.map (fun _ -> MlVar asst) spec.ml_asst in
     let vasstF = Option.map (fun _ -> MlVar asstFinal) spec.ml_asstFinal in
     let tr_ges e = match tr e vev_step spec.ml_cfg0 vasst None with
       | MlTupleLst [e'; _] -> e'
       | _ -> raise (Invalid_argument "Unexpected translated term")
     in
     let ges' = List.map (fun (name, e) -> (name, tr_ges e)) ges in
     let ges'' = [(ev_step, spec.ml_delta)] @ gasst @ gasstF @ ges' in
     let tr_main_body = tr main_body vev_step spec.ml_cfg0 vasst vasstF in
     MlGDefs (ges'', Some (MlGDefMain (MlRec (fopt, xs, tr_main_body))))
  | _, _ -> raise (Invalid_argument "Either Main is missing or the Prop file is not provided") 
     
