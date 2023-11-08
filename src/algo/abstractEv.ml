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
  print_endline "\n\nassert:";
  print_exp stdout spec.asst;
  print_endline "\n\ninitial config:";
  print_exp stdout spec.cfg0;
  print_endline "\n<-------------------->\n"

let parse_property_spec prop_file = 
  let spec = parse_aut_spec prop_file in
  (print_aut_spec spec);
  spec

type evenv = EmptyEvEnv | ExtendEvEnv of var * relation_e * evenv
let empty_env () = EmptyEvEnv
let extend_env id v env = ExtendEvEnv (id, v, env)
let rec apply_env var = function
  | EmptyEvEnv -> raise (Invalid_argument (var^ " not found! when evaluating EV"))
  | ExtendEvEnv (id, v, tail) -> if var = id then v else apply_env var tail
let rec fold_env f env acc = match env with
  | EmptyEvEnv -> acc
  | ExtendEvEnv (id, v, tail) -> f id v (fold_env f tail acc) 

let eff_minimize = StateMap.filter (fun _ a -> not @@ is_bot_R a)
let eff_bot spec = List.fold_right (fun q e -> StateMap.add q (bot_R Plus) e) spec.qset (StateMap.empty)
let eff0 () =
  Option.map 
    (fun spec -> 
      match spec.cfg0 with 
              | TupleLst ([e1; e2], _) -> 
                 begin match e1, e2 with
                 | Const ((Integer q0), _), Const (acc0, _) -> 
                    StateMap.mapi (fun (Q q) acc -> if q = q0 then init_R_c acc0 else acc) (eff_bot spec)
                 | _, _ -> eff_bot spec
                 end
              | _ -> eff_bot spec
    ) (!property_spec) 
  |> (fun mspec -> Option.value mspec ~default:(StateMap.empty))
  |> eff_minimize

(* todo: use memoization for traversing only once the transition function expression tree  *)
let ev eff t = 
  
  let spec = match !property_spec with 
    | Some s -> s
    | None -> raise (Invalid_argument "Property spec file not found")
  in

  let add_tran vq va ts = (arrow_R "q'" va vq)::ts in
  let env0 = EmptyEvEnv 
             |> extend_env "q" (top_R Plus)
             |> extend_env "acc" (top_R Plus) 
             |> extend_env "evx" (top_R Plus) 
             |> extend_env "q'" (top_R Plus)
  in
  let name_of_node l = "ze" ^ l in
  let get_env_vars env = fold_env (fun id v lst -> id::lst) env [] in
  let eff_bot = eff_bot spec in
  let rec eval e env ae ts = 
    match e with
    | Const (c, _) -> 
       let v = init_R_c c |> (fun v -> stren_R v ae) in (v, ts)
    | Var (x, _) -> 
       let v = apply_env x env
               |> (fun v -> if sat_equal_R v x then v else equal_R (forget_R x v) x)
               |> (fun v -> stren_R v ae)
       in 
       (v, ts)
    | Ite (e0, e1, e2, _, _) -> 
       let v0, ts' = eval e0 env ae ts in
       (* (if !Config.debug then Format.printf "\nITE_e0: "; pr_relation Format.std_formatter v0; Format.printf "\n";); *)
       begin match v0 with
       | Bool _ -> 
          let aet = meet_R ae (extrac_bool_R v0 true) in
          let v1, ts'' = eval e1 env aet ts' in
          (* (if !Config.debug then Format.fprintf Format.std_formatter "\nITE_e1: %a\nv: " (pr_exp true) e1; 
           pr_relation Format.std_formatter v1; Format.printf "\n";); *)
          let aef = meet_R ae (extrac_bool_R v0 false) in 
          let v2, ts''' = eval e2 env aef ts'' in
          (* (if !Config.debug then Format.fprintf Format.std_formatter "\nITE_e2: %a\nv: " (pr_exp true) e2; 
           pr_relation Format.std_formatter v2; Format.printf "\n";); *)
          begin match v1, v2 with 
          | Unit _, Unit _ -> (Unit (), ts''') 
          | Int _, Int _ -> ((join_R v1 v2), ts''')
          | Bool _, Bool _ -> ((join_R v1 v2), ts''')
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
        let v1, ts' = eval e1 env ae ts in 
        (* (if !Config.debug then Format.printf "\nBinOp_e1: "; pr_relation Format.std_formatter v1; Format.printf "\n";);  *)
        let v2, ts'' = eval e2 env ae ts' in
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
            ((stren_R raw_v ae), ts'')
          end 
    | UnOp (uop, e1, _) ->
       let v1, ts' = eval e1 env ae ts in
       (* (if !Config.debug then Format.printf "\nUOp_e1: "; pr_relation Format.std_formatter v1; Format.printf "\n";); *)
       
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
           ((stren_R raw_v ae), ts')
         end
    | TupleLst ([e1; e2], _) ->
       let vq, _ = eval e1 env ae ts in
       let va, _ = eval e2 env ae ts in
       (* (Format.fprintf Format.std_formatter "\ne: %a,@ vq: %a,@ va: %a" (pr_exp true) e pr_relation vq pr_relation va); *)
       let ts' = begin match vq with 
                   | Unit _ -> ts 
                   | _ -> add_tran vq va ts 
                 end 
       in
       (* ( Format.printf "\nts: ";
         Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_relation Format.std_formatter ts';
         Format.printf "\n";); *)
       (Unit (), ts')
    | _ -> (Unit (), ts) 
  in
  let _, ts = eval spec.delta env0 (top_R Plus) [] in  (* memoize this *)
  (* let pp_eff e = 
    let ppf = Format.std_formatter in
    if StateMap.is_empty e then Format.fprintf ppf "Empty\n" 
    else StateMap.bindings e 
         |> Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_eff_binding ppf
  in *)
  ( if !Config.debug then
      begin 
        Format.printf "\nts: ";
        Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_relation Format.std_formatter ts;
        Format.printf "\n";
      end);
  (* (Format.printf "\neff_bot: "; (pp_eff eff_bot););
  (Format.printf "\neff: "; (pp_eff eff)); *)
  eff_minimize @@ StateMap.mapi (fun (Q q') _ -> 
      let ra' = 
        StateMap.fold 
          (fun (Q q) a res ->
            (* (Format.fprintf Format.std_formatter "\na: %a,@ res: %a" pr_relation a pr_relation res); *)
            let a' = 
              List.fold_right (fun va res' -> 
                  (* (Format.fprintf Format.std_formatter "\nres(init): %a,@,va(init): %a" pr_relation res' pr_relation va); *)
                  let va' = arrow_R "acc" va a in
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- a]: %a" pr_relation va');*)
                  let va' = arrow_R "q" va' (init_R_c (Integer q)) in
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- q(%d)]: %a" q  pr_relation va'); *)
                  let va' = arrow_R "evx" va' t in
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- evx]: %a" pr_relation va'); *)
                  let va' = arrow_R "q'" va' (init_R_c (Integer q')) in
                  (* (Format.fprintf Format.std_formatter "\nva[acc <- q'(%d)]: %a" q' pr_relation va');*)
                  let res'' = join_R res' va' in
                  (* (Format.fprintf Format.std_formatter "\nres(final): %a" pr_relation res'');*)
                  res''
                ) ts (bot_R Plus)
            in
            (* (Format.fprintf Format.std_formatter "\nq(%d) -> q'(%d): %a" q q' pr_relation a'); *)
            join_R res a'
          ) eff (bot_R Plus)
        |> forget_R "q"
        |> forget_R "q'" 
      in
      (if !Config.debug then Format.fprintf Format.std_formatter "\nq'(%d): %a" q' pr_relation ra');
      ra'
    ) eff_bot
