open Syntax
open EffectAutomataSyntax
open Printer

let property_spec: (string * aut_spec) option ref = ref None

let evfn_var = "ev"
let evasstfn_var = "ev_assert"

let parse_aut_spec file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let a = aut_spec_of_ml (EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf) false in 
  let a_raw = (seek_in chan 0; really_input_string chan (in_channel_length chan)) in
  let _ = close_in chan in
  (a_raw, a)

let parse_property_spec prop_file = 
  let spec = parse_aut_spec prop_file in
  spec

let fresh_k_q_acc_gen: (unit -> kvar * qvar * accvar) option ref = ref None
let fresh_k_q_acc () = match !fresh_k_q_acc_gen with
  | Some f -> f ()
  | None -> let f () = begin match Option.map snd !property_spec with
                   | Some aspec -> 
                      begin match aspec.env_vars with
                      | _::acc -> let accvars = List.map (fun accx -> fresh_var accx) acc in
                                 (fresh_var "k", fresh_var "q", accvars)
                      | _ -> raise (Invalid_argument "Expected Ev arguments")
                      end
                   | None -> raise (Invalid_argument "Expected property file")
                   end in
           fresh_k_q_acc_gen := Some f; f ()

let ev_assert_flag: bool option ref = ref None
let ev_assert_on () = 
  match !ev_assert_flag with
    | Some v -> v
    | None -> let v = begin match Option.map snd !property_spec with
                     | None -> false
                     | Some spec -> Option.map (fun _ -> true) spec.asst 
                                   |> (fun o -> Option.value o ~default:false)
                     end in
             ev_assert_flag := Some v; v
   
let rec cps_convert: term -> qvar -> accvar -> (qvar -> accvar -> xvar -> kterm) -> kterm = 
  fun e q acc tr_k -> 
  (* (Format.fprintf Format.std_formatter "@.%a" (pr_exp false) e); *)
  match e with 
  | Const (c,_) -> 
     let x = fresh_var "x" in
     KLetVal (x, KConst c, tr_k q acc [x])
  | Var (x,_) -> let x = if x = "main" then "_main" else x in tr_k q acc [x]
  | Rec (None, (x,_), e, _) -> 
     begin match e with 
     | PatMat (Var _, [Case (TupleLst ([Var (x1, _); Var (x2, _)], _), e')], _) ->
        let f = fresh_var "f" in
        let k0, q0, acc0 = fresh_k_q_acc () in
        let ke' = cps_convert e' q0 acc0 (fun q0' acc0' res0' -> KContApp (k0, q0', acc0', res0')) in
        KLetVal (f, KFn (k0, q0, acc0, [x1;x2], ke'), tr_k q acc [f])
     | _ ->
        let f = fresh_var "f" in
        let x = if x = "main" then "_main" else x in
        let k0, q0, acc0 = fresh_k_q_acc () in
        let ke' = cps_convert e q0 acc0 (fun q0' acc0' res0' -> KContApp (k0, q0', acc0', res0')) in
        KLetVal (f, KFn (k0, q0, acc0, [x], ke'), tr_k q acc [f])
     end
  | Rec (Some (f,_), (x,_), e, _) ->
     begin match e with
     | PatMat (Var _, [Case (TupleLst ([Var (x1, _); Var (x2, _)], _), e')], _) ->
        let k0, q0, acc0 = fresh_k_q_acc () in
        let ke' = cps_convert e' q0 acc0 (fun q0' acc0' res0' -> KContApp (k0, q0', acc0',res0')) in
        KFix (f, k0, q0, acc0, [x1; x2], ke', tr_k q acc [f])
     | _ -> 
        let k0, q0, acc0 = fresh_k_q_acc () in
        let ke' = cps_convert e q0 acc0 (fun q0' acc0' res0' -> KContApp (k0, q0', acc0', res0')) in
        KFix (f, k0, q0, acc0, [x], ke', tr_k q acc [f])
     end
  | App (e1, e2, _) ->
     let k0, q0, acc0 = fresh_k_q_acc () in
     let res0 = fresh_var "res" in
     begin match e2 with
     | TupleLst ([e2_1;e2_2], _) ->
        cps_convert e1 q acc (fun q1' acc1' res1' ->
            cps_convert e2_1 q1' acc1' (fun q2_1' acc2_1' res2_1' -> 
                cps_convert e2_2 q2_1' acc2_1' (fun q2_2' acc2_2' res2_2' -> 
                    KLetCont 
                      (k0, q0, acc0, res0, (tr_k q0 acc0 [res0]),
                       KApp (List.hd res1', k0, q2_2', acc2_2', res2_1' @ res2_2')))))
     | _ ->
        cps_convert e1 q acc (fun q1' acc1' res1' ->
            cps_convert e2 q1' acc1' (fun q2' acc2' res2' ->
                KLetCont 
                  (k0, q0, acc0, res0, (tr_k q0 acc0 [res0]),
                   KApp (List.hd res1', k0, q2', acc2', res2'))))
     end
  | Ite (e0, e1, e2, _) -> 
     let k0, q0, acc0 = fresh_k_q_acc () in
     let res0 = fresh_var "res" in
     let k1, q1, acc1 = fresh_k_q_acc () in
     let res1 = fresh_var "res" in
     let k2, q2, acc2 = fresh_k_q_acc () in
     let res2 = fresh_var "res" in
     cps_convert e0 q acc (fun q0' acc0' res0' ->
         KLetCont 
           (k0, q0, acc0, res0, (tr_k q0 acc0 [res0]),
            KLetCont 
              (k1, q1, acc1, res1, 
               (cps_convert e1 q1 acc1 (fun q1' acc1' res1' -> KContApp (k0, q1', acc1', res1'))),
               KLetCont 
                 (k2, q2, acc2, res2, 
                  (cps_convert e2 q2 acc2 (fun q2' acc2' res2' -> KContApp (k0, q2', acc2', res2'))),
                  KIte (List.hd res0', KContApp (k1, q0', acc0', res0'), KContApp (k2, q0', acc0', res0'))
       ))))
  | UnOp (unop, e, _) -> 
     let x = fresh_var "x" in
     cps_convert e q acc (fun q' acc' res' -> KLetUnOp (x, unop, List.hd res', (tr_k q' acc' [x])))
  | BinOp (binop, e1, e2, _) -> 
     let x = fresh_var "x" in 
     cps_convert e1 q acc (fun q1' acc1' res1' ->
         cps_convert e2 q1' acc1' (fun q2' acc2' res2' -> 
             KLetBinOp (x, binop, List.hd res1', List.hd res2', (tr_k q2' acc2' [x]))))
  | Event (e, _) -> 
     let k0, q0, acc0 = fresh_k_q_acc () in
     let x = fresh_var "x" in 
     let xunit = fresh_var "x" in
     let get_ktevapp k q acc res = if ev_assert_on () then KEvAssertApp (k, q, acc, res)
                                   else KEvApp (k, q, acc, res) in 
     cps_convert e q acc (fun q' acc' res' ->
         KLetVal (xunit, KConst (UnitLit), 
                  KLetCont (k0, q0, acc0, x, (tr_k q0 acc0 [xunit]),
                            (get_ktevapp k0 q' acc' (List.hd res')))))
  | Assert (e, _, _) ->
     let xunit = fresh_var "x" in
     cps_convert e q acc (fun q' acc' res' ->
         KLetVal (xunit, KConst (UnitLit),
                  KAssert (List.hd res', (tr_k q' acc' [xunit]))))
  | TupleLst([e1;e2], _) -> 
     cps_convert e1 q acc (fun q1' acc1' res1' -> 
         cps_convert e2 q1' acc1' (fun q2' acc2' res2' -> 
             tr_k q2' acc2' (res1' @ res2')))
  | NonDet _ -> 
     let x = fresh_var "x" in
     let zero = fresh_var "x" in
     let nondet_bool = fresh_var "bool_random" in
     KLetVal (x, KRandomInt, 
              KLetVal (zero, KConst (Integer 0), 
                       KLetBinOp (nondet_bool, Ge, x, zero, (tr_k q acc [nondet_bool]))))  
  | e -> (Format.fprintf Format.std_formatter "ERROR, missing pattern for exp: @.%a" (pr_exp false) e); raise (Invalid_argument "ERROR")  
   
let get_init_config e = 
  match e with
  | TupleLst ((Const (cq,_))::eacc, _) -> 
     let cacc = begin match eacc with 
                | [Const (c, _)] -> [c]
                | [UnOp (UMinus, (Const (Integer c, _)), _)] -> [Integer (-c)]
                | [TupleLst (accs, _)] -> 
                   List.map (function 
                       | Const (c, _) -> c 
                       | UnOp (UMinus, (Const (Integer c, _)), _) -> Integer (-c)
                       | _ -> raise (Invalid_argument "AccInit expected to be a constant"))
                     accs
                | _ -> raise (Invalid_argument ("AccInit not valid. " ^ 
                                                 "Expected a constant or a tuple of constants"))
                end in
     (cq, cacc)
  | _ -> raise (Invalid_argument "ConfigInit expected to be a tuple of constants")

let run e =
  let mk_k_app ev_k eq eacc =  
    let acc_args = match eacc with
      | TupleLst (eaccs, _) -> eaccs
      | _ -> [eacc] in
    let q_arg = eq in
    let res_arg = Const (UnitLit, "") in
    List.fold_left (fun e1 earg -> mk_app e1 earg) (mk_var ev_k) ((q_arg::acc_args) @ [res_arg]) in
  let rec cps_convert_ev k e = 
    match e with
    | TupleLst ([eq; eacc], _) -> mk_k_app k eq eacc
    | Ite (e0, e1, e2, l) -> Ite (e0, cps_convert_ev k e1, cps_convert_ev k e2, l)
    | _ -> e
  in
  match !property_spec with
  | None -> failwith "Could not file property specification"
  | Some (_, aspec) ->
     let ev_k = fresh_var "k" in
     let ev_q = "q" in
     let ev_x, ev_acc = begin match aspec.env_vars with
                        | x::acc -> x, acc
                        | _ -> raise (Invalid_argument "Expected Ev arguments")
                        end in
     let q_init, acc_init = get_init_config aspec.cfg0 in
     let eva_k, eva_q, eva_acc = fresh_k_q_acc () in
     let eva_x = fresh_var "x" in 
     let cps_prog = 
       let _, qi0, acci0 = fresh_k_q_acc () in
       let kt_prog = cps_convert e qi0 acci0 (fun q' acc' res' ->
                         begin match aspec.asstFinal with
                         | None -> KExit (List.hd res')
                         | Some asstFinal ->
                            let k0 = fresh_var "k" in
                            let x0 = fresh_var "x" in
                            KLetCont (k0, ev_q, ev_acc, x0,
                                      (cps_convert asstFinal ev_q ev_acc (fun _ _ res'' ->
                                           KAssert (List.hd res'', KExit x0))),
                                      KContApp (k0, q', acc', res'))
                         end) in
       List.fold_right2 (fun x c ktin -> 
           KLetVal (x, KConst c, ktin)) (qi0::acci0) (q_init::acc_init) kt_prog in
     let cps_ev_asst asst = 
       let xunit = fresh_var "x" in
       let k0 = fresh_var "k" in
       let x0 = fresh_var "x" in
       let ktdef = KLetCont (k0, ev_q, ev_acc, x0,
                             (cps_convert asst ev_q ev_acc (fun q' acc' res' ->
                                  KLetVal (xunit, KConst UnitLit,
                                           KAssert (List.hd res', KContApp (eva_k, q', acc', [xunit]))))),
                             KApp (evfn_var, k0, eva_q, eva_acc, [eva_x])) in
       KLetVal (evasstfn_var, KFn (eva_k, eva_q, eva_acc, [eva_x], ktdef), 
                cps_prog) in
     let cps_ev = 
       let cps_evkt = begin match aspec.asst with
                      | Some asst -> cps_ev_asst asst
                      | None -> cps_prog
                      end in
       KLetVal (evfn_var, 
                KFn (ev_k, ev_q, ev_acc, [ev_x], KExp (cps_convert_ev ev_k aspec.delta)),
                cps_evkt)
     in  
     let prefvars = VarDefMap.bindings !Syntax.pre_vars |> List.rev_map fst in
     KMainDef(prefvars, cps_ev)
