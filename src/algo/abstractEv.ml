open Syntax
open EffectAutomataSyntax
open Printer
open Util
open SensitiveDomain
open AbstractDomain
open SemanticDomain
open SemanticsDomain

(* todo: 
   1. Bop and Uop abstract operators 
   2. Define a new set of semantic actions for the EffectAutomata Grammar 
*)

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

let spec prop_file = parse_aut_spec prop_file

type evenv = EmptyEvEnv | ExtendEvEnv of var * relation_e * evenv
let empty_env () = EmptyEvEnv
let extend_env id v env = ExtendEvEnv (id, v, env)
let rec apply_env var = function
  | EmptyEvEnv -> raise (Invalid_argument (var^ " not found! when evaluating EV"))
  | ExtendEvEnv (id, v, tail) -> if var = id then v else apply_env var tail

(* todo: use memoization for traversing only once the transition function expression tree  *)
let ev e_delta ae eff t asst = 
  
  let add_tran vq va ts = (arrow_R "q'" va vq)::ts in

  let rec eval e env ae ts = 
    match e with
    | Const (c, _) -> 
       let v = init_R_c c |> (fun v -> stren_R v ae) in (v, ts)
    | Var (x, _) -> 
       (* todo: do not look in the env for the 3 predefined variables: [q; a; v] *)
       let v = apply_env x env
               |> (fun v -> if sat_equal_R v x then v else equal_R (forget_R x v) x)
               |> (fun v -> stren_R v ae)
       in 
       (v, ts)
    | Ite (e0, e1, e2, _, _) -> 
       let v0, _ = eval e0 env ae ts in
       begin match v0 with
       | Bool _ -> 
          let aet = meet_R ae (extrac_bool_R v0 true) in
          let v1, ts' = eval e1 env aet ts in
          let aef = meet_R ae (extrac_bool_R v0 false) in 
          let v2, ts'' = eval e2 env aef ts' in
          begin match v1, v2 with 
          | Unit _, Unit _ -> ts'' 
          | Int _, Int _ -> ((join_R v1 v2), ts'')
          | Bool _, Bool _ -> ((join_R v1 v2), ts'')
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
       
       op_R bop v1 v2 
    | UnOp (uop, e1, _) ->
       let v1 = eval e1 env ae ts in
       uop_R op v1
    | TupleLst ([e1; e2], _) ->
       let vq, _ = eval e1 env ae ts in
       let va, _ = eval e2 env ae ts in
       (Unit unit, (add_tran vq va ts))
    | _ -> ts 
  in
  let ts = eval e_delta EmptyEvEnv (top_R Plus) [] in  (* memoize this *)
  let eff_bot = List.fold_right (fun q e -> StateMap.add q (bot_R plus) e) qset (StateMap.empty) in
  StateMap.map (fun q' _ -> 
      List.fold (fun q a ->
          List.fold (fun va res' -> 
              let va' = arrow_R va "a" a in
              let va' = arrow_R va' "q" (init_R_c q) in
              let va' = arrow_R va' "ev" t in
              let va' = arrow_R va' "q'" (init_R_c q') in 
              join_R res va'
            ) ts (bot_R Plus)
        ) eff (bot_R Plus)
    ) eff_bot
