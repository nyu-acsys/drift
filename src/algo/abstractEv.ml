open Syntax
open EffectAutomataSyntax
open Printer
open Util
open SensitiveDomain
open AbstractDomain
open SemanticDomain
open SemanticsDomain

let property_spec: aut_spec option ref = ref None

let parse_aut_spec file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  EffectAutomataGrammar.top EffectAutomataLexer.token lexbuf

let print_aut_spec (spec:aut_spec) = 
  print_endline "\nProperty Automaton Spec:";
  print_endline "\nQSet:";
  print_endline ("[" ^ (String.concat ";" (List.map (fun (Q q) -> string_of_int q) spec.qset)) ^ "]");
  print_endline "\ndelta:";
  print_exp stdout spec.delta;
  print_endline "\nenv:";
  print_endline ("{" ^ (String.concat ";" spec.env) ^ "}");
  print_endline "\nassert:";
  (match spec.asst with Some asst -> print_exp stdout asst | None -> Format.printf "None");
  print_endline "\nassertFinal:";
  (match spec.asstFinal with Some asst -> print_exp stdout asst | None -> Format.printf "None");
  print_endline "\n\ninitial config:";
  print_exp stdout spec.cfg0;
  print_endline "\n<-------------------->\n"

let parse_property_spec prop_file = 
  let spec = parse_aut_spec prop_file in
  (print_aut_spec spec);
  spec

type evv_t = Val of relation_e | Tpl of evv_t list
type evenv = EmptyEvEnv | ExtendEvEnv of var * relation_e * evenv

let empty_env () = EmptyEvEnv
let extend_env id v env = ExtendEvEnv (id, v, env)
let rec apply_env var = function
  | EmptyEvEnv -> raise (Invalid_argument (var^ " not found! when evaluating EV"))
  | ExtendEvEnv (id, v, tail) -> if var = id then v else apply_env var tail
let rec fold_env f env acc = match env with
  | EmptyEvEnv -> acc
  | ExtendEvEnv (id, v, tail) -> f id v (fold_env f tail acc) 

let pr_acc ppf acc = 
  let pr_acc_comp ppf (var, va) = Format.fprintf ppf "%s |-> %a" var pr_relation va in
  VarMap.bindings acc 
  |> Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_acc_comp ppf 
 
let is_bot_acc acc = VarMap.fold (fun _ va res -> if res then is_bot_R va else res) acc true
let eff_minimize = StateMap.filter (fun _ acc -> not @@ is_bot_acc acc)
let eff_bot spec = 
  let acc_vars = List.filter (fun v -> v != "evx") spec.env in
  let bot_acc = List.fold_right (fun v acc -> VarMap.add v (bot_R Plus) acc) acc_vars VarMap.empty in
  List.fold_right (fun q e -> StateMap.add q bot_acc e) spec.qset (StateMap.empty)
let eff0 () = 
  let acc0_of_exp spec e =
    let acc_vars = List.filter (fun v -> v <> "evx") spec.env in
    let accv_of_idx i = List.nth acc_vars i in 
    match e with 
    | Const (acc0, _) -> 
       List.fold_right (fun accv acc -> VarMap.add accv (init_R_c acc0) acc) acc_vars VarMap.empty
    | TupleLst (es, _) -> 
       List.mapi (fun i e -> ((accv_of_idx i), e)) es
       |> (fun ves -> List.fold_right
                     (fun (accv, e) acc -> match e with
                                        | Const (acc0, _) -> VarMap.add accv (init_R_c acc0) acc
                                        | _ -> acc)
                     ves VarMap.empty)
    | _ -> VarMap.empty
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
  |> (fun mspec -> Option.value mspec ~default:(StateMap.empty))
  |> eff_minimize

let ev: effect_t -> relation_t -> effect_t = fun eff t -> 
  
  let spec = match !property_spec with 
    | Some s -> s
    | None -> raise (Invalid_argument "Property spec file not found")
  in
  let absv_of_val = function 
    | Val v -> v
    | _ -> raise (Invalid_argument "Expected an abstract value")
  in
  let find v acc = VarMap.find_opt v acc |> Opt.get_or_else (top_R Plus) in
  let acc_vars = List.filter (fun v -> v <> "evx") spec.env in
  let get_env0 q acc t = EmptyEvEnv 
                         |> extend_env "q" (init_R_c (Integer q))
                         |> extend_env "evx" t
                         |> (List.fold_right (fun v e -> extend_env v (find v acc) e) acc_vars)
                         |> (VarDefMap.fold (fun v _ e -> extend_env v (top_R Plus) e) !pre_vars) 
  in
  let bot_acc = List.fold_right (fun v acc -> VarMap.add v (bot_R Plus) acc) acc_vars VarMap.empty in
  let arrow_acc var acc v = VarMap.map (fun va -> arrow_R var va v) acc in
  let join_acc acc1 acc2 = VarMap.merge 
                             (fun v mva1 mva2 -> match mva1, mva2 with 
                                              | Some va1, Some va2 -> Some (join_R va1 va2) 
                                              | _, _ -> None)
                             acc1 acc2
  in 
  let forget_acc v acc = VarMap.map (fun va -> forget_R v va) acc in
  let accvar_of_idx i = List.nth acc_vars i in
  let acc_of_evv = function
    | Val va -> VarMap.map (fun _ -> va) bot_acc
    | Tpl vvs -> 
       begin
         List.mapi (fun i vva -> ((accvar_of_idx i), vva)) vvs
         |> List.fold_left 
              (fun acc (accv, vva) -> match vva with 
                                   | Val va -> VarMap.add accv va acc
                                   | _ -> acc) 
              VarMap.empty
         |> (fun acc' -> join_acc acc' bot_acc) 
       end 
  in
  let add_tran vq vva ts =
    let rec arrow_va_q' = function
      | Val va -> Val (arrow_R "q'" va vq)
      | Tpl vvs -> Tpl (List.map arrow_va_q' vvs)
    in
    let vva' = arrow_va_q' vva in
    (acc_of_evv vva')::ts
  in
  let name_of_node l = "ze" ^ l in
  let get_env_vars env = fold_env (fun id v lst -> id::lst) env [] in
  let eff_bot = eff_bot spec in
  let rec eval e env ae tftpl ts = 
    match e with
    | Const (c, _) -> 
       let v = init_R_c c |> (fun v -> stren_R v ae) in (Val v, ts)
    | Var (x, _) -> 
       let v = apply_env x env
               |> (fun v -> if sat_equal_R v x then v else equal_R (forget_R x v) x)
               |> (fun v -> stren_R v ae)
       in 
       (Val v, ts)
    | Ite (e0, e1, e2, _, _) -> 
       let vv0, ts' = eval e0 env ae tftpl ts in
       let v0 = absv_of_val vv0 in
       (* (if !Config.debug then Format.printf "\nITE_e0: "; pr_relation Format.std_formatter v0; Format.printf "\n";); *)
       begin match v0 with
       | Bool _ -> 
          let aet = meet_R ae (extrac_bool_R v0 true) in
          let vv1, ts'' = eval e1 env aet tftpl ts' in
          let v1 = absv_of_val vv1 in
          (* (if !Config.debug then Format.fprintf Format.std_formatter "\nITE_e1: %a\nv: " (pr_exp true) e1; 
           pr_relation Format.std_formatter v1; Format.printf "\n";); *)
          let aef = meet_R ae (extrac_bool_R v0 false) in 
          let vv2, ts''' = eval e2 env aef tftpl ts'' in
          let v2 = absv_of_val vv2 in
          (* (if !Config.debug then Format.fprintf Format.std_formatter "\nITE_e2: %a\nv: " (pr_exp true) e2; 
           pr_relation Format.std_formatter v2; Format.printf "\n";); *)
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
        (* (if !Config.debug then 
           begin
             Format.printf "\nEV: BinOp \n";
             pr_exp true Format.std_formatter e;
             Format.printf "\n";
           end); *)
        let vv1, ts' = eval e1 env ae tftpl ts in
        let v1 = absv_of_val vv1 in  
        (* (if !Config.debug then Format.printf "\nBinOp_e1: "; pr_relation Format.std_formatter v1; Format.printf "\n";);  *)
        let vv2, ts'' = eval e2 env ae tftpl ts' in
        let v2 = absv_of_val vv2 in
        (* (if !Config.debug then Format.printf "\nBinOp_e2: "; pr_relation Format.std_formatter v2; Format.printf "\n";); *)
        if is_unit_R v1 || is_unit_R v2 then 
          raise (Invalid_argument ("Binary operator " ^ (string_of_op bop) ^ " is not defined for tuples"))
        else begin
            let bop = begin match bop, e1, e2 with
                      | Mod, Const _, Const _ -> Mod
                      | Mod, _, Const _ -> Modc
                      | _ -> bop
                      end 
            in
            let raw_v = 
              begin match bop with
              | And | Or -> bool_op_R bop v1 v2
              | Modc ->
                 let op1 = e1 |> loc |> name_of_node in
                 let v' = top_R Plus |> (fun v -> arrow_R op1 v v1) in
                 let v'' = op_R "" op1 (str_of_const e2) bop false v' in
                 get_env_vars env |> proj_R v''
              | _ ->
                 let op1 = e1 |> loc |> name_of_node in
                 let op2 = e2 |> loc |> name_of_node in
                 let v' = top_R Plus |> (fun v -> arrow_R op1 v v1) |> (fun v -> arrow_R op2 v v2) in
                 (* (if !Config.debug then Format.printf "\nBinOp_[op1 <- v1, op2 <- v2]:"; 
                  pr_relation Format.std_formatter v'; Format.printf "\n";); *)
                 let v'' = op_R "" op1 op2 bop false v' in
                 (* (if !Config.debug then Format.printf "\nBinOp_op_result:"; 
                  pr_relation Format.std_formatter v''; Format.printf "\n";);   *)
                 let v''' = get_env_vars env |> proj_R v'' in
                 (* (if !Config.debug then Format.printf "\nBinOp_Proj_env:"; 
                    pr_relation Format.std_formatter v'''; Format.printf "\n";); *)
                  v'''
              end
            in
            (Val (stren_R raw_v ae), ts'')
          end 
    | UnOp (uop, e1, _) ->
       let vv1, ts' = eval e1 env ae tftpl ts in
       (* (if !Config.debug then Format.printf "\nUOp_e1: "; pr_relation Format.std_formatter v1; Format.printf "\n";); *)
       let v1 = absv_of_val vv1 in
 
       if is_unit_R v1 then
         raise (Invalid_argument ("Unary operator " ^ (string_of_unop uop) ^ " is not defined for tuples"))
       else begin
           let op1 = e1 |> loc |> name_of_node in
           let v' = top_R Plus |> (fun v -> arrow_R op1 v v1) in
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
       let tftpl' = if tftpl then false else tftpl in

       let vvq, _ = eval e1 env ae tftpl' ts in
       let vva, _ = eval e2 env ae tftpl' ts in
       (* (Format.fprintf Format.std_formatter "\ne: %a,@ vq: %a,@ va: %a" (pr_exp true) e pr_relation vq pr_relation va); *)
       let vv', ts' = begin match tftpl with 
                 | true -> begin
                     let vq = absv_of_val vvq in
                     match vq with
                          | Unit _ -> (Val (Unit ()), ts)
                          | _ -> (Val (Unit ()), (add_tran vq vva ts))
                          end
                 | false -> (Tpl [vvq;vva], ts)
                 end
       in
       (* ( Format.printf "\nts: ";
         Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_relation Format.std_formatter ts';
         Format.printf "\n";); *)
       (vv', ts')
    | TupleLst (es, _) ->
       let vv' = List.map (fun e -> let ve, _ = eval e env ae tftpl ts in ve) es in
       (Tpl vv', ts)
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
  eff_minimize @@ StateMap.mapi (fun (Q q') _ -> 
      let ra' = 
        StateMap.fold 
          (fun (Q q) acc res' ->
            let env0 = get_env0 q acc t in
            let _, ts = eval spec.delta env0 (top_R Plus) true [] in
            ( if !Config.debug then
                begin 
                  Format.printf "\nts: ";
                  Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_acc Format.std_formatter ts;
                  Format.printf "\n";
                end);
            (* (Format.fprintf Format.std_formatter "\na: %a,@ res: %a" pr_relation a pr_relation res); *)
            let acc' = 
              List.fold_right (fun acc'' res'' -> 
                  (* (Format.fprintf Format.std_formatter "\nres(init): %a,@,va(init): %a" pr_relation res' pr_relation va); *)
                  (* let va' = arrow_R "acc" va a in *)
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- a]: %a" pr_relation va');*)
                  (* let va' = arrow_R "q" va' (init_R_c (Integer q)) in *)
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- q(%d)]: %a" q  pr_relation va'); *)
                  (* let va' = arrow_R "evx" va' t in *)
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- evx]: %a" pr_relation va'); *)
                  let acc'' = arrow_acc "q'" acc'' (init_R_c (Integer q')) in
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- q'(%d)]: %a" q' pr_relation va');*)
                  let res'' = join_acc res'' acc'' in
                  (* (Format.fprintf Format.std_formatter "\nres(final): %a" pr_relation res'');*)
                  res''
                ) ts bot_acc
            in
            (* (Format.fprintf Format.std_formatter "\nq(%d) -> q'(%d): %a" q q' pr_relation a'); *)
            join_acc res' acc'
          ) eff bot_acc
        |> forget_acc "q"
        |> forget_acc "q'" 
      in
      (if !Config.debug then Format.fprintf Format.std_formatter "\nq'(%d): %a" q' pr_acc ra');
      ra'
    ) eff_bot
