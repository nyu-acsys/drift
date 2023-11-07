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

let eff_bot spec = List.fold_right (fun q e -> StateMap.add q (bot_R Plus) e) spec.qset (StateMap.empty)
let eff0 spec = 
  match spec.cfg0 with 
    | TupleLst ([e1; e2], _) -> 
       begin match e1, e2 with
       | Const ((Integer q0), _), Const (acc0, _) -> 
          StateMap.mapi (fun (Q q) acc -> if q = q0 then init_R_c acc0 else acc) (eff_bot spec)
       | _, _ -> eff_bot spec
       end
    | _ -> eff_bot spec
  
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
             |> extend_env "evv" (top_R Plus) 
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
       begin match v0 with
       | Bool _ -> 
          let aet = meet_R ae (extrac_bool_R v0 true) in
          let v1, ts'' = eval e1 env aet ts' in
          let aef = meet_R ae (extrac_bool_R v0 false) in 
          let v2, ts''' = eval e2 env aef ts'' in
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
       let v1, ts' = eval e1 env ae ts in 
       let v2, ts'' = eval e2 env ae ts' in
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
              let v'' = op_R "" op1 op2 bop false v' in
              get_env_vars env |> proj_R v''
           end
         in
         ((stren_R raw_v ae), ts'')
         end 
    | UnOp (uop, e1, _) ->
       let v1, ts' = eval e1 env ae ts in
       if is_unit_R v1 then
         raise (Invalid_argument ("Unary operator " ^ (string_of_unop uop) ^ " is not defined for tuples"))
       else begin
           let op1 = e1 |> loc |> name_of_node in
           let v' = top_R Plus |> (fun v -> arrow_R op1 v v1) in
           let v'' = uop_R "" uop op1 false v' in
           let raw_v = get_env_vars env |> proj_R v'' in
           ((stren_R raw_v ae), ts')
         end
    | TupleLst ([e1; e2], _) ->
       let vq, _ = eval e1 env ae ts in
       let va, _ = eval e2 env ae ts in
       (Unit (), (add_tran vq va ts))
    | _ -> (Unit (), ts) 
  in
  let _, ts = eval spec.delta env0 (top_R Plus) [] in  (* memoize this *)
  StateMap.mapi (fun (Q q') _ -> 
      StateMap.fold (fun (Q q) a res ->
          let a' = List.fold_right (fun va res' -> 
                       let va' = arrow_R "acc" va a in
                       let va' = arrow_R "q" va' (init_R_c (Integer q)) in
                       let va' = arrow_R "evv" va' t in
                       let va' = arrow_R "q'" va' (init_R_c (Integer q')) in 
                       join_R res' va'
                     ) ts (bot_R Plus) 
          in
          join_R res a'
        ) eff (bot_R Plus)
    ) eff_bot
