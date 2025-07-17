open Util
open Syntax
open EffectAutomataSyntax
open Ev_cfa

(* Selective Store Translation of effectful program into product program using tuple encoding: 

  - args: the annotated expression tree
  --- 
  alg. steps: 
   - add the ev_step function; include here the ev assertion
   - translate the expressions
   - identify the main function and use a let binding for the body to capture the result
   - insert the final assertion

  ---
  output: 
   - translate the expression
*)

exception TransError of string

let property_spec: (string * ml_aut_spec) option ref = ref None

let mk_fresh_var = compose (fun x -> MlVar x) fresh_var

let tr: fterm -> mlterm -> mlterm -> mlterm option -> mlterm option -> mlterm =
  fun e ev_step ecfg asst asstFinal -> 
  let ret e a cfg = match a with Fact.Val (_, Fact.ETop) -> MlTupleLst [e; cfg] | _ -> e in    
  let rec tr_ (e: fterm) (ecfg : mlterm) =
    let get_trtuple e ecfg k =
      let is_ev = function
        | MlLetIn (x, MlApp (MlApp ((MlVar "ev_step0"), _), _), MlApp ((MlVar "ev_step_asst0"), _)) ->
           true
        | _ -> false
      in
      match ann e with
      | Fact.(Val (_, CertNot)) -> k (tr_ e ecfg, ecfg)
      | Fact.(Val(_, ETop)) ->
         let tr_e = tr_ e ecfg in
         begin match tr_e with
         | MlTupleLst [e'; ecfg'] -> k (e', ecfg')
         | MlLetIn (x, def, body) when is_ev tr_e ->
            let e', ecfg' = k (MlConst UnitLit, MlVar x) in
            (MlLetIn (x, def, MlBinOp (Seq, body, e')), ecfg')
         | _ ->
            let ex, ecfgx = mk_fresh_var "x", mk_fresh_var "cfg" in
            let e', ecfg' = k (ex, ecfgx) in
            (MlPatMat (tr_e, [MlCase (MlTupleLst [ex; ecfgx], e')]), ecfg')           
         end
      | _ -> failwith (Format.asprintf "Analysis fact not found for %a@." Ev_cfa.pr_fterm e)
    in
    match e with
    | AnnConst (c, a) -> ret (MlConst c) a ecfg

    | AnnVar (x, a) -> ret (MlVar x) a ecfg

    | AnnRec (fopt, x, def, a) ->
       begin match a with
       | Fact.Val (Fact.TFun (t1, t2, Fact.ETop), _) ->
          let fc = mk_fresh_var "cfg" in
          ret (MlRec (fopt, [MlVar x; fc], tr_ def fc)) a ecfg
       | Fact.Val (Fact.TFun (t1, t2, Fact.CertNot), _) ->
          ret (MlRec (fopt, [MlVar x], tr_ def ecfg)) a ecfg
       | _ -> failwith "Analysis Fact not found for AnnRec"
       end

    | AnnApp (e1, e2, a) ->
       let is_fun = function
         | AnnRec (_, _, _, _) -> true
         | _ -> false
       in
       let is_not_fun_and_E = function
         | AnnRec (_, _, _, Fact.(Val (_, ETop))) -> false
         | _ -> true
       in
       begin match e1, e2 with
       (* chase App that have been the conversion of a LetIn *)
       | AnnRec (None, x, e1_body, (Val (TFun (v1i, v1o, a1), CertNot))), _
            when not @@ is_fun e1_body && is_not_fun_and_E e2 ->
          fst @@get_trtuple e2 ecfg (fun (e2', ecfg2') ->
                    (MlLetIn (x, e2', tr_ e1_body ecfg2'), mk_fresh_var "dummy_cfg"))
       
       (* fallback to app*)
       | _ -> 
             fst @@ get_trtuple e1 ecfg (fun (e1', ecfg1') ->
                        get_trtuple e2 ecfg1' (fun (e2', ecfg2') ->
                            let dummy_cfg = mk_fresh_var "dummycfg" in
                            match (ann e1) with
                            | Fact.(Val (TFun (_, _, CertNot), _)) ->
                               (ret (MlApp (e1', e2')) a ecfg2', dummy_cfg)
                            | Fact.(Val (TFun (_, _, ETop), _)) ->
                               (MlApp (MlApp (e1', e2'), ecfg2'), dummy_cfg)
                            | _ -> failwith "Analysis Fact invalid for e1 in AnnAp (e1, e2, a)" ))
       end

    | AnnUnOp (uop, e1, a) ->
       fst @@ get_trtuple e1 ecfg (fun (e1', ecfg1') ->                           
                  (ret (MlUnOp (uop, e1')) a ecfg1', mk_fresh_var "dummycfg"))

    | AnnBinOp (bop, e1, e2, a) ->
       fst @@ get_trtuple e1 ecfg (fun (e1', ecfg1') ->
                           get_trtuple e2 ecfg1' (fun (e2', ecfg2') ->
                               (ret (MlBinOp (bop, e1', e2')) a ecfg2', mk_fresh_var "dummycfg" ))) 

    | AnnIte (e0, e1, e2, a) ->
      fst @@ get_trtuple e0 ecfg (fun (e0', ecfg0') ->
                 (MlIte (e0', tr_ e1 ecfg0', tr_ e2 ecfg0'), mk_fresh_var "dummy_cfg"))

    | AnnNonDet a -> ret MlNonDet a ecfg

    | AnnEvent (e1, a) ->
       fst @@ get_trtuple e1 ecfg (fun (e1', ecfg1') ->
                  (* Format.fprintf Format.std_formatter "@[Event: %a, (e1'=%a, ecfg1'=%a)@]@." Ev_cfa.pr_fterm e1 Printer.pr_mlterm e1' Printer.pr_mlterm ecfg1'; *)
                  match asst with
                  | None -> 
                     (MlTupleLst [MlConst UnitLit; MlApp (MlApp (ev_step, e1'), ecfg1')], mk_fresh_var "dummy_cfg")
                  | Some asst ->
                     let ecfgx = mk_fresh_var "cfg" in
                     let ecfgx_name = match ecfgx with MlVar x -> x | _ -> failwith "Expected a variable" in
                     (MlLetIn (ecfgx_name, MlApp (MlApp (ev_step, e1'), ecfg1'),
                               MlTupleLst [MlApp (asst, ecfgx); ecfgx]), ecfgx)
                     
                     (* let ecfgx = mk_fresh_var "cfg" in *)
                     (* (MlApp (MlRec (None, [ecfgx], *)
                     (*                MlBinOp (Seq, MlApp (asst, ecfgx), MlTupleLst [MlConst UnitLit; ecfgx])), *)
                     (*         MlApp (MlApp (ev_step, e1'), ecfg1')), *)
                     (*  mk_fresh_var "dummy_cfg") *)
                )
 
    | AnnAssert (e1, pos, a) ->
       fst @@ get_trtuple e1 ecfg (fun (e1', ecfg1') ->
                  (ret (MlAssert (e1', pos)) a ecfg1', mk_fresh_var "dummy_cfg"))

    | AnnTupleLst (es, a) ->
       let rec fold_ = function
         | [], es', ecfg' -> (ret (MlTupleLst (List.rev es')) a ecfg', mk_fresh_var "dummy_cfg")
         | ei::eis, es', ecfg' -> get_trtuple ei ecfg' (fun (ei', ecfgi') -> fold_ (eis, (ei'::es'), ecfgi'))
       in
       fst @@ fold_ (es, [], ecfg)     
              
    | AnnPatMat (e1, patlst, a) ->
       fst @@ get_trtuple e1 ecfg (fun (e1', ecfg1') ->
                  let tr_case = function 
                    | AnnCase (AnnConst (c, _), ec) -> MlCase (MlConst c, tr_ ec ecfg1')
                    | AnnCase (AnnVar (x, _), ec) -> MlCase (MlVar x, tr_ ec ecfg1')  
                    | AnnCase (AnnTupleLst (pes, _), ec) ->
                       let pes' = 
                         List.map (fun pe -> 
                             match pe with 
                             | AnnVar (x, _) -> MlVar x 
                             | _ -> failwith "Pattern Matching with tuple pattern: only vars supported") pes
                       in 
                       MlCase (MlTupleLst pes', tr_ ec ecfg1')
                    | _ -> failwith "Pattern Matching: not supported patterns other than const/var/tuples of vars"
                  in
                  (MlPatMat (e1', List.map tr_case patlst), mk_fresh_var "dummy_cfg"))
  
  in
  let tr_e = match asstFinal with
    | None -> tr_ e ecfg
    | Some asst -> 
       let e', ecfg' = mk_fresh_var "e", mk_fresh_var "cfg" in
       MlPatMat (tr_ e ecfg, [
             MlCase (MlTupleLst [e'; ecfg'],
                     MlBinOp (Seq, MlApp (asst, ecfg'), e'))])
  in
  tr_e

let rec gdefs_of_ea ea acc = 
  let ges, main_opt = acc in
  match ea with
  | AnnApp (AnnRec (None, f, e1, a1), ((AnnRec (None, _, _, _)) as fe), fa) ->
     if (f = "main") then
       if main_opt = None then gdefs_of_ea e1 (ges, Some fe) else failwith "Duplicated 'main' not allowed"
     else
       gdefs_of_ea e1 ((f, fe)::ges, main_opt)
  | AnnApp (AnnRec (None, f1, e1, a1), ((AnnRec (Some f2, _, _, _)) as fe), fa) when f1 = f2 ->
     if (f1 = "main") then
       if main_opt = None then gdefs_of_ea e1 (ges, Some fe) else failwith "Duplicated 'main' not allowed"
     else
       gdefs_of_ea e1 ((f1, fe)::ges, main_opt)
  | _ -> (List.rev ges, main_opt)

let tr_gdef (f, ea) ev_step ecfg asst =
  let open Fact in
  let has_eff (a: Fact.t) = 
    match a with
    | Val (TFun (_, _, Fact.CertNot), _) -> false
    | Val (TFun (_, _, Fact.ETop), _) -> true
    | _ -> failwith "Effect Annotation expected for Fun shape"
  in
  let rec collect_params ea ps = 
    match ea with
    | AnnRec (None, x, e', a) when not @@ has_eff a -> collect_params e' (x::ps)
    | _ -> (List.rev ps, ea)
  in
  match ea with
  | AnnRec (None, x, e, a) ->       
     let ps, body = collect_params ea [] in
     let tr_body = tr body ev_step (mk_fresh_var "dummy_cfg") asst None in
     begin match tr_body with
     | MlRec (None, xs, body) -> MlRec (None, (List.map (fun x -> MlVar x) ps) @ xs, body)
     | _ -> MlRec (None, (List.map (fun x -> MlVar x) ps), tr_body)
     end
  | AnnRec (Some f, x, e, a) ->       
     let ps, body = collect_params (AnnRec (None, x, e, a)) [] in
     let tr_body = tr body ev_step (mk_fresh_var "dummy_cfg") asst None in
     begin match tr_body with
     | MlRec (None, xs, body) -> MlRec (Some f, (List.map (fun x -> MlVar x) ps) @ xs, body)
     | _ -> MlRec (Some f, (List.map (fun x -> MlVar x) ps), tr_body)
     end
  | _ -> failwith "GDef must be a function"

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

let fv_spec acc e = 
  let rec fv bvs acc = function
    | MlConst _ -> acc
    | MlVar x -> if StringSet.mem x bvs then acc else StringSet.add x acc
    | MlRec (f_opt, xs, e) ->
       let bvs1 = Option.map (fun f -> StringSet.add f bvs) f_opt |> Opt.get_or_else bvs in
       let bvs2 = List.fold_left (fun acc e -> fv StringSet.empty acc e) bvs1 xs in
       fv bvs2 acc e 
    | MlApp (e1, e2) | MlBinOp (_, e1, e2) -> List.fold_left (fv bvs) acc [e1; e2]
    | MlUnOp (_, e) -> fv bvs acc e
    | MlIte (e1, e2, e3) -> List.fold_left (fv bvs) acc [e1; e2; e3]
    | MlPatMat (e1, ps) ->
       let acc1 = fv bvs acc e1 in
       List.fold_left (fun acc (MlCase (p, ce)) -> let bvs1 = fv StringSet.empty bvs p in fv bvs1 acc ce) acc1 ps 
    | MlTupleLst es -> List.fold_left (fv bvs) acc es
    | MlEvent e | MlAssert (e, _) -> fv bvs acc e
    | MlNonDet -> acc
    | MlLetIn (x, e1, e2) ->
       fv bvs acc e1 |> (fun acc' -> fv (StringSet.add x bvs) acc' e2)
    | MlGDefs _ | MlGDefMain _ -> failwith "MlGDef or MlGDefMain should not occur in the specs"
  in
  fv StringSet.empty acc e

let run (mle: mlterm) = 
  (* Run analysis after converting mlterm into term and labeling all sub-expressions *)
  let e = term_of_ml mle in
  let e = label e in
  Logs.debug (fun m -> m "Prog:@,@[%a@]@.@.@." (Printer.pr_exp true) e);
  let ea = annotate_eff e in
  
  let (ges, main_opt) = gdefs_of_ea ea ([], None) in
  (* Run translation back into mlterm. Use the types to guide the translation  *)
  let ev_step = fresh_var "ev_step" in 
  let asst = fresh_var "ev_step_asst" in
  let asstFinal = fresh_var "asst_final" in

  match !property_spec  with
  | Some (s_, spec) ->
     let gasst = Option.map (fun e -> [(asst, e)]) spec.ml_asst
                 |> Option.value ~default:[] in
     let gasstF = Option.map (fun e -> [(asstFinal, e)]) spec.ml_asstFinal
                  |> Option.value ~default:[] in
     let vev_step = MlVar ev_step in
     let vasst = Option.map (fun _ -> MlVar asst) spec.ml_asst in
     let vasstF = Option.map (fun _ -> MlVar asstFinal) spec.ml_asstFinal in
     Logs.debug (fun m -> m "AnnTerm before tr: @[%a@]@." Ev_cfa.pr_fterm ea);     
     (* global defs *)
     let ges' = List.map (fun gdef -> (fst gdef, tr_gdef gdef vev_step spec.ml_cfg0 vasst)) ges in
     (* main *)
     let rec destruct_main ea acc = 
       let xs, _ = acc in
       match ea with
       | AnnRec (f_opt, x, e, a) ->
          destruct_main e (x::xs, e)
       | _ -> (List.rev xs, ea)
     in
     let main = 
       begin match main_opt with  
       | Some ((AnnRec (f_opt, x, e, a)) as f) -> 
          let xs, body = destruct_main f ([], f) in
          let tr_body = tr body vev_step spec.ml_cfg0 vasst vasstF in
          MlRec (f_opt, List.map (fun x -> MlVar x) xs, tr_body)
       | _ -> failwith "Invalid main term"
       end in
     let ges'' = [(ev_step, spec.ml_delta)] @ gasst @ gasstF @ ges' in
     Logs.debug (fun m -> m "tr_main : @[%a@]@." Printer.pr_mlterm main);
     MlGDefs (ges'', Some (MlGDefMain main))
     (* MlGDefs (ges'', Some (MlGDefMain (MlRec (Some "main", [], tr_main_body)))) *)
  | None -> raise (Invalid_argument "Prop file is not provided")


  
