open Config
open Syntax
open EffectAutomataSyntax
open Printer
open Util
open SensitiveDomain
open AbstractDomain
open SemanticDomain
open SemanticsDomain


type evv_t = Val of relation_e
type evenv = EmptyEvEnv | ExtendEvEnv of var * relation_e * evenv

type eff_assert_class = EvAssert of loc | FinalAssert 

let acc_name = "acc"
let property_spec: aut_spec option ref = ref None
let program_pre_vars: relation_t VarDefMap.t ref = ref VarDefMap.empty 

let parse_aut_spec file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let a = EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf in 
  let _ = close_in chan in
  a

let print_aut_spec (spec:aut_spec) = 
  print_endline "\nProperty Automaton Spec:";
  print_endline "\nQSet:";
  print_endline ("[" ^ (String.concat ";" (List.map (fun (Q q) -> string_of_int q) spec.qset)) ^ "]");
  print_endline "\ndelta:";
  print_exp stdout spec.delta;
  print_endline "\nenv:";
  print_endline ("{" ^ (String.concat ";" spec.env_vars) ^ "}");
  print_endline "\nassert:";
  (match spec.asst with Some asst -> print_exp stdout asst | None -> Format.printf "None");
  print_endline "\nassertFinal:";
  (match spec.asstFinal with Some asst -> print_exp stdout asst | None -> Format.printf "None");
  print_endline "\n\ninitial config:";
  print_exp stdout spec.cfg0;
  print_endline "\n<-------------------->\n"

let parse_property_spec prop_file = 
  let spec = parse_aut_spec prop_file in
  (if !out_put_level < 2 then print_aut_spec spec);
  spec

let spec_env_vars: unit -> var list = fun () -> 
  Option.map (fun spec -> spec.env_vars) (!property_spec)
  |> (fun mvars -> Option.value mvars ~default:[])

let empty_env () = EmptyEvEnv
let extend_env id v env = ExtendEvEnv (id, v, env)
let rec apply_env var = function
  | EmptyEvEnv -> raise (Invalid_argument (var^ " not found! when evaluating EV"))
  | ExtendEvEnv (id, v, tail) -> if var = id then v else apply_env var tail
let rec fold_env f env acc = match env with
  | EmptyEvEnv -> acc
  | ExtendEvEnv (id, v, tail) -> f id v (fold_env f tail acc) 


let pr_delta_tran ppf t_acc =
  Format.fprintf ppf "\ntran added:@,  @[%a@]@." pr_eff_acc t_acc
let pr_delta_trans ppf ts =
  let rec pr_ts i ppf = function
    | [] -> Format.fprintf ppf ""
    | t_acc::ts' -> Format.fprintf ppf "@[ts(%d) |-> @[<v>%a,@]@]@,%a" i pr_eff_acc t_acc (pr_ts (i+1)) ts'
  in
  Format.fprintf ppf "\ndelta_trans:@,  @[@[<v>%a@]@]@." (pr_ts 1) ts

let pr_ev_env ppf env =
  let rec pr_env ppf = function
    | EmptyEvEnv -> Format.fprintf ppf ""
    | ExtendEvEnv (id, v, tail) -> Format.fprintf ppf "@[<hov 0>%s |-> %a,@]@ %a" id pr_relation v pr_env tail
  in
  Format.fprintf ppf "[ @[<v>%a@] ]@." pr_env env

let eff_bot spec = 
  let bot_acc = VarMap.add acc_name (bot_R Plus) VarMap.empty in
  List.fold_right (fun q e -> StateMap.add q bot_acc e) spec.qset (StateMap.empty)

let eff0 () = 
  let acc0_of_exp spec e =
    let acc_vars = List.filter (fun v -> v <> "evx") spec.env_vars in
    let accv_of_idx i = List.nth acc_vars i in 
    let acc = match e with 
      | Const (acc0, _) -> 
         List.fold_left (fun acc v -> arrow_R v acc (init_R_c acc0)) (top_R Plus) acc_vars
      | UnOp (UMinus, Const (Integer (acc0), _), _) -> 
         List.fold_left (fun acc v -> arrow_R v acc (init_R_c (Integer (-acc0)))) (top_R Plus) acc_vars
      | TupleLst (es, _) -> 
         List.mapi (fun i e -> ((accv_of_idx i), e)) es
         |> List.fold_left 
              (fun acc (v, e) -> match e with
                              | Const (acc0, _) -> arrow_R v acc (init_R_c acc0)
                              | UnOp (UMinus, Const (Integer (acc0), _), _) ->
                                  arrow_R v acc (init_R_c (Integer (-acc0)))
                              | _ -> acc)
              (top_R Plus)
      | _ -> (top_R Plus)
    in 
    VarMap.add acc_name acc VarMap.empty 
  in
  Option.map 
    (fun spec -> 
      match spec.cfg0 with 
              | TupleLst ([e1; e2], _) -> 
                 begin match e1 with
                 | Const ((Integer q0), _) -> StateMap.add (Q q0) (acc0_of_exp spec e2) StateMap.empty
                 | _ -> eff_bot spec
                 end
              | _ -> eff_bot spec
    ) (!property_spec) 
  |> (fun meff -> Option.value meff ~default:(StateMap.empty))
  |> minimize_eff

let get_ae_from_eff: effect_t -> SenSemantics.value_tt = fun eff ->
  StateMap.fold (fun (Q q) acc res ->
    let qae = VarMap.find acc_name acc in
    let spec = Option.get !property_spec in
    let qae' = List.fold_left (fun ae var ->
        forget_R var ae
        ) qae (spec.env_vars)
    in
    join_V (Relation qae') res
    ) eff Bot

let ev: var list -> effect_t -> relation_t -> effect_t = fun penv eff t -> 
  
  (if !debug then
     begin
     Format.fprintf Format.std_formatter 
       "\nEV (env, eff, t) =@.  @[env_vars:@[{ %s }@]@]@.  @[eff:@[%a@]@]@.  @[t:@[%a@]@]@."
       (String.concat "; " penv)
       pr_eff_map eff
       pr_relation t
     end);
  
  let spec = match !property_spec with 
    | Some s -> s
    | None -> raise (Invalid_argument "Property spec file not found")
  in
  let absv_of_val (Val v) = v in
  let acc_vars = List.filter (fun v -> v <> "evx") spec.env_vars in
  let find_acc_var x = List.find_opt ((=) x) acc_vars in
  let get_env0 q acc t = EmptyEvEnv 
                         |> extend_env "q" (init_R_c (Integer q))
                         |> extend_env "evx" t
                         |> (VarDefMap.fold (fun v va e -> extend_env v va e) !program_pre_vars)
                         |> VarMap.fold (fun v va e -> extend_env v va e) acc
  in
  let get_ae0 env = 
    let arrow_var x v = apply_env x env |> (fun xv -> arrow_R x v xv) in
    apply_env acc_name env |> arrow_var "q" |> arrow_var "evx" 
  in   
  let bot_acc = VarMap.add acc_name (bot_R Plus) VarMap.empty in
  let arrow_acc var acc v = VarMap.map (fun va -> arrow_R var va v) acc in
  let join_acc acc1 acc2 = VarMap.merge 
                             (fun v mva1 mva2 -> match mva1, mva2 with 
                                              | Some va1, Some va2 -> Some (join_R va1 va2) 
                                              | _, _ -> None)
                             acc1 acc2
  in 

  let forget_acc v acc = VarMap.map (fun va -> forget_R v va) acc in
  let acc_of_evv = function Val va -> VarMap.add acc_name va VarMap.empty in

  let add_tran vva ts =
    let forget_evx_var (Val va) = Val (forget_R "evx" va) in  
    let vva' = forget_evx_var vva in
    (if !Config.debug then
        let Val v = vva' in
        Format.fprintf Format.std_formatter "@.add_tran.vva': @[%a@]" pr_relation v);
    let vva_acc = acc_of_evv vva' in
    (if !debug then pr_delta_tran Format.std_formatter vva_acc);
    (vva_acc)::ts 
  in
  let name_of_node l = "ze" ^ l in
  let get_env_vars env = 
    fold_env (fun id _ lst -> if not @@ List.mem id lst then id::lst else lst) env penv 
  in
  let eff_bot = eff_bot spec in
  let parallel_assign_acc acc e = 
    let parse_legal_expr = function 
      | Const _ as c -> (str_of_const c, None, None)
      | Var (x, _) -> (x, None, None) 
      | BinOp (bop, (Var (x1,_)), (Var (x2,_)), _) -> (x1, Some x2, Some bop)
      | BinOp (bop, (Var (x1,_)), (Const _ as c2), _) -> (x1, Some (str_of_const c2), Some bop)
      | BinOp (bop, (Const _ as c1), (Var (x2, _)), _) -> (str_of_const c1, Some x2, Some bop)
      | BinOp (bop, (Const _ as c1), (Const _ as c2), _) -> (str_of_const c1, Some (str_of_const c2), Some bop)
      | _ -> failwith "Not supported" 
    in
    let assign_eabs = match e with 
      | TupleLst (es, _) -> List.map parse_legal_expr es 
      |  _ -> [parse_legal_expr e]
    in
    (if !Config.debug then 
       (Format.fprintf Format.std_formatter "@.pre_parallel_update.acc:@[%a@]" pr_relation acc));
    parallel_assign_R acc_vars assign_eabs acc 
  in
  let rec eval e env ae ts = 
    let check_mod_const e_mod = 
      match e_mod with
      | Const (Integer 2, _) -> true
      | _ -> false
    in
    (* ( Format.printf "\n>>>exp:"; pr_exp true Format.std_formatter e;
       Format.printf "\n>>>ae:"; pr_relation Format.std_formatter ae; Format.printf "\n";
       Format.printf "\n>>>env: "; pr_ev_env Format.std_formatter env); *)
    if is_bot_R ae then (Val (Unit ()), ts) else
    match e with
    | Const (c, _) -> 
       let v = init_R_c c |> (fun v -> 
           let v' = stren_R v ae in 
           (*(Format.printf "\nc:"; pr_const Format.std_formatter c; 
             Format.printf "\t c_post:"; pr_relation Format.std_formatter v'); *) v')
       in (Val v, ts)
    | Var (x, _) -> 
       let v = 
         (match find_acc_var x with
          | None -> apply_env x env
                   |> (fun v -> if sat_equal_R v x then v else equal_R (forget_R x v) x)
          | Some accx -> 
             apply_env acc_name env 
             |> (fun v -> 
              (if !Config.debug then 
                 Format.fprintf Format.std_formatter "@.var[%s]: @[%a@]" acc_name pr_relation v);
              let v' = equal_R v accx in 
              (if !Config.debug then 
                 Format.fprintf Format.std_formatter "@.var[%s]: @[%a@]" accx pr_relation v');
              v'))
         |> (fun v -> 
           (if !Config.debug then
              Format.fprintf Format.std_formatter "@.var_pre_stren[%s]:@[%a@]" x pr_relation v);
           let v'= stren_R v ae in 
           (if !Config.debug then
              Format.fprintf Format.std_formatter "@.var_post_stren[%s]:@[%a@]" x pr_relation v');
           v')
       in  
       (Val v, ts)
    | Ite (e0, e1, e2, _) -> 
       let vv0, ts' = eval e0 env ae ts in
       let v0 = absv_of_val vv0 in
       (if !Config.debug then 
         Format.fprintf Format.std_formatter "@.ITE_e0: @[%a@]@ :@[%a@]@." 
           (pr_exp false) e0 
           pr_relation v0);
       begin match v0 with
       | Bool _ -> 
          let aet = meet_R ae (extrac_bool_R v0 true) in
          let vv1, ts'' = eval e1 env aet ts' in
          let v1 = absv_of_val vv1 in
          (if !Config.debug then 
             Format.fprintf Format.std_formatter "@.ITE_e1: @[%a@]@. :@[%a@]" 
               (pr_exp true) e1
               pr_relation v1);
          let aef = meet_R ae (extrac_bool_R v0 false) in 
          let vv2, ts''' = eval e2 env aef ts'' in
          let v2 = absv_of_val vv2 in
          (if !Config.debug then 
             Format.fprintf Format.std_formatter "@.ITE_e2: @[%a@]@. :@[%a@]" 
               (pr_exp true) e2 
               pr_relation v2);
          begin match v1, v2 with 
          | Unit _, Unit _ -> (Val (Unit ()), ts''') 
          | Int _, Int _ -> (Val (join_R v1 v2), ts''')
          | Bool _, Bool _ -> (Val (join_R v1 v2), ts''')
          | _, _ -> raise (Invalid_argument ("Branches should either both evaluate to "^
                                              "values or pairs of next state and new acc"))
          end
       | _ -> raise (Invalid_argument "Should be a Boolean expression")
       end
    (* | Let (var, def, body) -> 
       let vdef, ts' = eval def env ae ts in
       begin match vdef with
       | Int _ | Bool _ ->
       eval body (extend_env var vdef env) ae ts'
       | Unit _ -> raise (Invalid_argument "Should be a value")
       end *)
    | BinOp (bop, e1, e2, _) -> 
       (if !Config.debug then 
          Format.fprintf Format.std_formatter "@.EV: BinOp @[%a@]"
            (pr_exp true) e);
       let vv1, ts' = eval e1 env ae ts in
       let v1 = absv_of_val vv1 in  
       (if !Config.debug then 
          Format.fprintf Format.std_formatter "@.BinOp_e1: @[%a@]" 
            pr_relation v1);
       let vv2, ts'' = eval e2 env ae ts' in
       let v2 = absv_of_val vv2 in
       (if !Config.debug then 
          Format.fprintf Format.std_formatter "@.BinOp_e2: @[%a@]" 
            pr_relation v2);
       if is_unit_R v1 || is_unit_R v2 then 
         raise (Invalid_argument ("Binary operator " ^ (string_of_op bop) ^ " is not defined for tuples"))
       else begin
           let bop = begin match bop, e1, e2 with
                     | Mod, Const _, Const _ -> Mod
                     | Mod, _, Const _ -> Modc
                     | _ -> bop
                     end 
           in
           let mod_eq_flag, e1, e2 = 
             match bop, e1, e2 with
             | Eq, BinOp (Mod, _, e_mod, _), _ -> check_mod_const e_mod, e1, e2
             | Eq, _, BinOp(Mod, _, e_mod, _) -> check_mod_const e_mod, e2, e1
             | Ne, BinOp(Mod, _, e_mod, _), _ -> check_mod_const e_mod, e1, e2
             | Ne, _, BinOp(Mod, _, e_mod, _) -> check_mod_const e_mod, e2, e1
             | _ -> false, e1, e2
           in
           (if !Config.debug then 
              Format.fprintf Format.std_formatter "@.Eval->mod_eq_flag: %B" mod_eq_flag);
           let raw_v = 
             begin match bop with
             | And | Or -> bool_op_R bop v1 v2
             | Modc ->
                let op1 = e1 |> loc |> name_of_node in
                let v' = top_R bop |> (fun v -> arrow_R op1 v v1) in
                let v'' = op_R "" op1 (str_of_const e2) bop false v' in
                get_env_vars env |> proj_R v''
             | _ ->
                let op1 = e1 |> loc |> name_of_node in
                let op2 = e2 |> loc |> name_of_node in
                let v' = top_R bop |> (fun v -> arrow_R op1 v v1) |> (fun v -> arrow_R op2 v v2) in
                (if !Config.debug then 
                   Format.fprintf Format.std_formatter "@.BinOp_[op1 <- v1, op2 <- v2]: @[%a@]" 
                     pr_relation v');
                let v'' = (if mod_eq_flag then op_R_eq_mod "" op1 op2 bop false v' 
                           else op_R "" op1 op2 bop false v') in
                (if !Config.debug then 
                   Format.fprintf Format.std_formatter "@.BinOp_op_result: @[%a@]" 
                     pr_relation v'');
                let v''' = get_env_vars env |> proj_R v'' in
                (if !Config.debug then 
                   Format.fprintf Format.std_formatter "@.BinOp_Proj_env: @[%a@]" 
                     pr_relation v''');
                v'''
             end
           in
           (Val (stren_R raw_v ae), ts'')
         end 
    | UnOp (uop, e1, _) ->
       let vv1, ts' = eval e1 env ae ts in
       (* (if !Config.debug then Format.printf "\nUOp_e1: "; pr_relation Format.std_formatter v1; Format.printf "\n";); *)
       let v1 = absv_of_val vv1 in
       
       if is_unit_R v1 then
         raise (Invalid_argument ("Unary operator " ^ (string_of_unop uop) ^ " is not defined for tuples"))
       else begin
           let op1 = e1 |> loc |> name_of_node in
           let v' = utop_R uop |> (fun v -> arrow_R op1 v v1) in
           (* (if !Config.debug then Format.printf "\nUOp_[op1 <- v1]:"; 
              pr_relation Format.std_formatter v'; Format.printf "\n";); *)
           let v'' = uop_R "" uop op1 false v' in
           (* (if !Config.debug then Format.printf "\nUOp_op_result:"; 
              pr_relation Format.std_formatter v''; Format.printf "\n";); *)
           let raw_v = get_env_vars env |> proj_R v'' in
           (* (if !Config.debug then Format.printf "\nUOp_Proj_env:"; 
              pr_relation Format.std_formatter raw_v; Format.printf "\n";); *)
           (Val (stren_R raw_v ae), ts')
         end
    | TupleLst ([e1; e2], _) -> 
       let (Val vq), _ = eval e1 env ae ts in
       (if !Config.debug then 
          Format.fprintf Format.std_formatter "@.q': @[%a@]" 
            pr_relation vq);
       let va = ae |> (fun v -> arrow_R "q'" v vq) in
       let (Val va) = Val (parallel_assign_acc va e2) in
       (if !Config.debug then 
          Format.fprintf Format.std_formatter "@.Parallel update result: @[%a@]" 
            pr_relation va);
       (Val (Unit ()), (add_tran (Val va) ts))
    | _ -> (Val (Unit ()), ts) 
  in
  (* let _, ts = eval spec.delta env0 (top_R Plus) [] in  (* memoize this *) *)
  (* let pp_eff e = 
     let ppf = Format.std_formatter in
     if StateMap.is_empty e then Format.fprintf ppf "Empty\n" 
     else StateMap.bindings e 
     |> Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_eff_binding ppf
     in *)
  
  (* (Format.printf "\neff_bot: "; (pp_eff eff_bot););
     (Format.printf "\neff: "; (pp_eff eff)); *)
  minimize_eff @@ 
    StateMap.mapi 
      (fun (Q q') _ -> 
        let ra' = 
          StateMap.fold 
            (fun (Q q) acc res' ->
              (if !debug then
                 Format.fprintf Format.std_formatter 
                   "\n@[EV(pre):@.  @[q[%d] |-> @[%a@]@]@]@." q pr_eff_acc acc
              );
              let env0 = get_env0 q acc t in
              let ae0 = get_ae0 env0 in
              let _, ts = eval spec.delta env0 ae0 [] in
              (if !debug then
                 pr_delta_trans Format.std_formatter ts
              );
              let acc' = 
                List.fold_right (fun acc'' res'' -> 
                    (if !debug then 
                       Format.fprintf Format.std_formatter "\nres(init): %a,@,acc(init): %a" pr_eff_acc res' pr_eff_acc acc);
                    let acc'' = arrow_acc "q'" acc'' (init_R_c (Integer q')) in
                    (if !debug then 
                       Format.fprintf Format.std_formatter "\nva[acc <- q'(%d)]: %a" q' pr_eff_acc acc'');
                    let res'' = join_acc res'' acc'' in
                     (if !debug then 
                        Format.fprintf Format.std_formatter "\nres(final): %a" pr_eff_acc res'');
                    res''
                  ) ts bot_acc
              in
              (if !debug then 
                 Format.fprintf Format.std_formatter "\nq(%d) -> q'(%d): %a" q q' pr_eff_acc acc');
              let res' = join_acc res' acc' in 
              (if !Config.debug then 
                 Format.fprintf Format.std_formatter 
                   "\n@[EV(post):@.  @[q[%d] |-> @[%a@]@]@]" q' pr_eff_acc res'
              );
              res'
            ) eff bot_acc
          |> forget_acc "q"
          |> forget_acc "q'" 
        in 
        (* (if !Config.debug then Format.fprintf Format.std_formatter "\nq'(%d): %a" q' pr_acc ra'); *)
        ra'
      ) eff_bot
  |> (fun eff -> 
    (if !debug then Format.fprintf Format.std_formatter "\nEff(post):@,@[%a@]@." pr_eff_map eff);
    eff)

let eval_assert term env acc_vars = 
  let find_acc_var x = List.find_opt ((=) x) acc_vars in
  let check_mod_const e_mod = 
    match e_mod with
    | Const (Integer 2, _) -> true
    | _ -> false
  in
  let rec eval term env =
    match term with
    | Const (c, _) -> init_R_c c 
    | Var (x, _) ->
       let v = 
         (match find_acc_var x with
          | None -> apply_env x env |> (fun v -> if sat_equal_R v x then v else equal_R (forget_R x v) x)
          | Some accx -> apply_env acc_name env|> (fun v -> equal_R v accx))
       in v
    (* apply_env x env *)
    (* |> (fun v -> if sat_equal_R v x then v else equal_R (forget_R x v) x) *)        
    | BinOp (bop, e1, e2, _) -> 
       let v1 = eval e1 env in
       let v2 = eval e2 env in
       if is_unit_R v1 || is_unit_R v2 then 
         raise (Invalid_argument ("Binary operator " ^ (string_of_op bop) ^ " is not defined for units"))
       else begin
           let bop = begin match bop, e1, e2 with
                     | Mod, Const _, Const _ -> Mod
                     | Mod, _, Const _ -> Modc
                     | _ -> bop
                     end 
           in
           let mod_eq_flag, e1, e2 = 
             match bop, e1, e2 with
             | Eq, BinOp (Mod, _, e_mod, _), _ -> check_mod_const e_mod, e1, e2
             | Eq, _, BinOp(Mod, _, e_mod, _) -> check_mod_const e_mod, e2, e1
             | Ne, BinOp(Mod, _, e_mod, _), _ -> check_mod_const e_mod, e1, e2
             | Ne, _, BinOp(Mod, _, e_mod, _) -> check_mod_const e_mod, e2, e1
             | _ -> false, e1, e2
           in 
            (if !Config.debug then 
               Format.fprintf Format.std_formatter "@.Assert->exp: @[%a@], mod_eq_flag: %B@." 
                 (pr_exp false) term mod_eq_flag);
           let raw_v = 
             begin match bop with
             | And | Or -> bool_op_R bop v1 v2
             | Modc ->
                let op1 = e1 |> loc |> name_of_node in
                let v' = top_R bop |> (fun v -> arrow_R op1 v v1) in
                let v'' = op_R "" op1 (str_of_const e2) bop false v' in
                v''
             | _ ->
                let op1 = e1 |> loc |> name_of_node in
                let op2 = e2 |> loc |> name_of_node in
                let v' = top_R bop |> (fun v -> arrow_R op1 v v1) |> (fun v -> arrow_R op2 v v2) in
                let v'' = (if mod_eq_flag then op_R_eq_mod "" op1 op2 bop false v'
                           else op_R "" op1 op2 bop false v') in
                v''
             end
           in
           raw_v
         end 
    | UnOp (uop, e1, _) ->
       let v1 = eval e1 env in
       
       if is_unit_R v1 then
         raise (Invalid_argument ("Unary operator " ^ (string_of_unop uop) ^ " is not defined for units"))
       else begin
           let op1 = e1 |> loc |> name_of_node in
           let v' = utop_R uop |> (fun v -> arrow_R op1 v v1) in
           let v'' = uop_R "" uop op1 false v' in
           v''
         end
    | _ -> bot_R Eq
  in 
  eval term env

let check_assert eac envE eff = 
  let pr_eff_assert_class ppf = function
    | EvAssert l -> Format.fprintf ppf "ev^%s" l
    | FinalAssert -> Format.fprintf ppf "final-eff"
  in
  let find_pre v = VarMap.find_opt v envE
                   |> Opt.get_or_else (top_R Plus) in 
  let asst_term spec = match eac with | EvAssert _ -> spec.asst | FinalAssert -> spec.asstFinal in
  Opt.map (fun spec -> 
      let acc_vars = List.filter (fun v -> v <> "evx") spec.env_vars in
      match asst_term spec with
      | None -> true
      | Some term ->
         StateMap.fold (fun (Q q) acc res ->
             if not res then res 
             else begin
                 let env = EmptyEvEnv 
                           |> extend_env "q" (init_R_c (Integer q))
                           |> VarMap.fold (fun v va e -> extend_env v va e) acc
                           |> (VarDefMap.fold (fun v _ e -> extend_env v (find_pre v) e) !pre_vars)
                 in
                 let r = eval_assert term env acc_vars in
                 
                 if not (is_bool_bot_R r) && not (is_bool_false_R r) then 
                   begin
                     (if !debug then 
                        begin 
                          Format.fprintf Format.std_formatter 
                            "\nASSERTION CHECKING (%a) ***FAILED***:@,@[<v>@[asst: %a@]@,@[env: %a@]@,@[res: %a@]@]@." 
                            pr_eff_assert_class eac (pr_exp true) term pr_ev_env env pr_relation r
                        end);
                     false
                   end
                 else if (is_bool_bot_R r) && (is_asst_false term) = false then 
                   begin
                      (if !debug then 
                         begin 
                           Format.fprintf Format.std_formatter 
                             "\nASSERTION CHECKING (%a) ***FAILED***:@,@[<v>@[asst: %a@]@,@[env: %a@]@,@[res: %a@]@]@." 
                             pr_eff_assert_class eac (pr_exp true) term pr_ev_env env pr_relation r
                         end);
                     false
                   end
                 else 
                   begin 
                     (if !debug then 
                        begin 
                          Format.fprintf Format.std_formatter 
                            "\nASSERTION CHECKING (%a) ===VALID===:@,@[<v>@[asst: %a@]@,@[env: %a@]@,@[res: %a@]@]@." 
                            pr_eff_assert_class eac (pr_exp true) term pr_ev_env env pr_relation r
                        end);
                      true
                   end 
               end) eff true 
    ) !property_spec
  |> Opt.get_or_else true
  
let check_assert_bot eac = 
  Opt.map (fun spec -> 
      match eac with 
      | EvAssert _ -> begin match spec.asst with None -> true | Some _ -> false end
      | FinalAssert -> begin match spec.asstFinal with None -> true | Some _ -> false end
    ) !property_spec
  |> Opt.get_or_else true

let check_assert_top eac = 
  Opt.map (fun spec -> 
      match eac with 
      | EvAssert _ -> true
      | FinalAssert -> true
    ) !property_spec
  |> Opt.get_or_else true
