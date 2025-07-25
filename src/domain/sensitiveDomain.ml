open AbstractDomain
open Syntax
open EffectAutomataSyntax
open Util
open Config
open TracePartDomain

exception AssertionError of string

let name_of_node lb = ("z" ^ lb)
let get_input_acc_name trace (Q q) var = var^"_"^trace^"_"^(string_of_int q)

type environment_t = AbstractValue.t

type relation_t = Int of AbstractValue.t 
  | Bool of AbstractValue.t * AbstractValue.t
  | Unit of AbstractValue.t
  | Env of environment_t
type array_t = (var * var) * (relation_t * relation_t)

module type HashType =
  sig
    type t
  end

module MakeHash(Hash: HashType) = struct
  type key = Hash.t
  type 'a t = (key, 'a) Hashtbl.t
  (* let empty : 'a t = Hashtbl.create 1234 *)
  let create (num: int) : 'a t = Hashtbl.create num
  let add key v (m: 'a t) : 'a t = Hashtbl.replace m key v; m
  let update key fn m =
    let v' = Hashtbl.find_opt m key |> fn in
    add key v' m
  let merge f m1 m2 : 'a t = 
    Hashtbl.filter_map_inplace (
    fun key v1 -> match Hashtbl.find_opt m2 key with
      | None -> None
      | Some v2 -> (f key v1 v2)
  ) m1; m1
  let union f (m1: 'a t) (m2: 'a t) : 'a t = 
    Hashtbl.iter (
    fun key v1 -> (match Hashtbl.find_opt m2 key with
      | None -> Hashtbl.add m2 key v1
      | Some v2 -> Hashtbl.replace m2 key (f key v1 v2))
  ) m1; m2
  let for_all f (m: 'a t) : bool = Hashtbl.fold (
    fun key v b -> if b = false then b else
      f key v
  ) m true
  let find_opt key (m: 'a t) = Hashtbl.find_opt m key
  let map f (m: 'a t) : 'a t = Hashtbl.filter_map_inplace (
    fun _ v -> Some (f v)
  ) m; m
  let mapi f (m: 'a t) : 'a t = Hashtbl.filter_map_inplace (
    fun key v -> Some (f key v)
  ) m; m
  let find key (m: 'a t): 'a = Hashtbl.find m key
  let bindings (m: 'a t) = Hashtbl.fold (
    fun key v lst -> (key, v) :: lst
  ) m []
  let fold = Hashtbl.fold
  let iter = Hashtbl.iter
end

exception Pre_Def_Change of string
exception Key_not_found of string

module TempVarMap =  Map.Make(struct
  type t = var
  let compare = compare
  end)

module VarMap = struct
  include TempVarMap
  let find key m = try TempVarMap.find key m 
    with Not_found -> raise (Key_not_found (key^" is not Found in VarMap"))
end

module TempENodeMap = Map.Make(struct
  type t = trace_t
  let compare = compare
end)

module TableMap = struct
  include TempENodeMap
  let find cs m = let trace = cs in
    try TempENodeMap.find cs m 
    with Not_found -> raise (Key_not_found ((get_trace_data trace)^"is not Found in TableMap"))
  let compare = compare
end

(* type state_t = state *)(* representation of automata state domain*)
type relation_e = relation_t (* dependent effect *)
type acc_t = relation_e
module StateMap = Map.Make(struct type t = state_t let compare (Q q1) (Q q2) = compare q1 q2 end)
type effect_t = acc_t  StateMap.t

module type SemanticsType =
  sig
    type node_t
    type node_s_t
    type env_t = (node_t * bool) VarMap.t
    type table_t
    type fout_t
    type value_tt =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and value_te = 
      | TEBot 
      | TETop
      | TypeAndEff of value_tt * eff
    and eff = EffBot | EffTop | Effect of effect_t
    and list_t = (var * var) * (relation_t * value_te)
    and tuple_t = value_te list
    val temap: (value_tt -> value_tt) * (eff -> eff) -> value_te -> value_te
    val effmap: (effect_t -> effect_t) -> eff -> eff
    val effmapi: (state_t -> relation_e -> relation_e) -> eff -> eff
    val init_fout: fout_t
    val init_T: trace_t -> relation_t -> table_t
    val set_env: relation_t -> table_t -> table_t
    val table_to_value: (relation_t -> bool) -> table_t -> value_tt
    val get_env_T: table_t -> relation_t
    val alpha_rename_fout: (value_te -> string -> string -> value_te) -> fout_t -> string -> string -> fout_t
    val alpha_rename_T: (value_te -> string -> string -> value_te) -> (relation_t -> string -> string -> relation_t) -> table_t -> string -> string -> table_t
    val join_T: (value_te -> value_te -> value_te) -> (relation_t -> relation_t -> relation_t) -> (value_te -> string -> string -> value_te) -> table_t -> table_t -> table_t
    val meet_T: (value_te -> value_te -> value_te) -> (relation_t -> relation_t -> relation_t) -> (value_te -> string -> string -> value_te) -> table_t -> table_t -> table_t
    val leq_T: (value_te -> value_te -> bool) -> (relation_t -> relation_t -> bool) -> table_t -> table_t -> bool
    val eq_T: (value_te -> value_te -> bool) -> (relation_t -> relation_t -> bool) -> table_t -> table_t -> bool
    val forget_T: (var -> value_te -> value_te) -> (var -> relation_t -> relation_t) -> var -> table_t -> table_t
    val arrow_T: (var -> value_tt -> value_tt) -> (var -> value_te -> value_tt -> value_te) -> (var -> value_tt -> value_tt -> value_tt) -> var -> table_t -> value_tt -> table_t
    val wid_T: (value_te -> value_te -> value_te) -> (relation_t -> relation_t -> relation_t) -> (value_te -> string -> string -> value_te) -> table_t -> table_t -> table_t
    (* val equal_T: (value_te -> var -> value_te) -> (value_te -> string -> string -> value_te) -> table_t -> var -> table_t *)
    val replace_fout: (value_te -> var -> var -> value_te) -> fout_t -> var -> var -> fout_t
    val replace_T: (value_te -> var -> var -> value_te) -> (relation_t -> var -> var -> relation_t) -> table_t -> var -> var -> table_t
    val stren_T: (value_te -> value_tt -> value_te) -> (value_tt -> value_tt -> value_tt) -> table_t -> value_tt -> table_t
    val proj_T: 
    (value_te -> (string list * state_t list) -> string list -> value_te) -> (relation_t -> string list -> relation_t) -> 
      (string -> value_te -> string list) -> table_t -> (string list * state_t list) -> string list -> table_t
    (* val bot_shape_T: (value_te -> value_te) -> (relation_t -> relation_t) -> table_t -> table_t *)
    val get_label_snode: node_s_t -> string
    val construct_vnode: env_t -> var -> trace_t -> node_t
    val construct_enode: env_t -> var -> node_t
    val construct_snode: trace_t -> node_t -> node_s_t
    val construct_fout: trace_t -> value_te -> fout_t
    val construct_table: trace_t -> value_te * fout_t -> relation_t -> table_t
    val v_fout: trace_t -> fout_t -> value_te
    val io_T: trace_t -> value_te -> value_te * fout_t
    val get_vnode: node_t -> env_t * var * trace_t
    val dx_T: value_te -> trace_t
    val get_fout_tails: fout_t -> trace_t list
    val trace_exists: trace_t -> table_t -> bool
    val get_table_T: value_te -> table_t
    val print_node: node_t -> Format.formatter -> (Format.formatter -> (string * (node_t * bool)) list -> unit) -> unit
    val print_fout: fout_t -> Format.formatter -> (Format.formatter -> value_te -> unit) -> unit
    val print_table: table_t -> Format.formatter -> (Format.formatter -> value_te -> unit) -> (Format.formatter -> relation_t -> unit) -> unit
    val compare_node: node_s_t -> node_s_t -> int
    val prop_table: 
    (trace_t -> value_te * value_te -> value_te * value_te -> (value_te * value_te) * (value_te * value_te)) -> 
      (value_te -> var -> var -> value_te) -> (relation_t -> relation_t -> relation_t) -> 
        table_t -> table_t -> table_t * table_t
    val step_func: (relation_t -> trace_t -> value_te * fout_t -> 'a -> 'a) -> value_te -> 'a -> 'a
    val get_full_fout: fout_t -> (trace_t * value_te) list
    val get_full_table_T: table_t -> (trace_t * (value_te * fout_t)) list * relation_t
    val get_table_by_cs_T: trace_t -> table_t -> (value_te * fout_t)
    val update_fout: trace_t -> value_te -> fout_t -> fout_t
    val merge_fout: (value_te -> value_te -> value_te * value_te) -> fout_t -> fout_t -> fout_t * fout_t
    val update_table: trace_t -> value_te * fout_t -> table_t -> table_t
    val get_name: node_s_t -> var
    val get_trace: node_s_t -> trace_t
    val is_top_fout: fout_t -> bool
    val table_isempty: table_t -> bool
    val table_mapi: (trace_t -> value_te * fout_t -> value_te * fout_t) -> table_t -> table_t
    val append_call_trace: (var -> trace_t -> trace_t)
    val append_part_trace: (partition_token_t -> trace_t -> trace_t)
    val extend_trace: (trace_t -> trace_t -> trace_t)
  end

module NonSensitive: SemanticsType =
  struct
    type node_t = EN of env_t * var (*N = E x loc*)
    and env_t = (node_t * bool) VarMap.t (*E = Var -> N*)
    type node_s_t = SN of bool * var
    type value_tt =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and eff = EffBot | EffTop | Effect of effect_t
    and value_te = 
      | TEBot 
      | TETop 
      | TypeAndEff of value_tt * eff 
    and fout_t = value_te
    and table_t = trace_t * value_te * fout_t * relation_t
    and list_t = (var * var) * (relation_t * value_te)
    and tuple_t = value_te list
    let temap (f, g) te = 
      let ddp = function TypeAndEff (Bot,EffBot) -> TEBot | TypeAndEff (Top,EffTop) -> TETop | te -> te in
      let dp = function TEBot -> TypeAndEff (Bot,EffBot) | TETop -> TypeAndEff (Top,EffTop) | te -> te in 
      match te with 
      | TypeAndEff (v, e) -> TypeAndEff (f v, g e) (* TypeAndEff (apply2 (f, g) ve)  *)      
      |  _ -> dp te |> (function TypeAndEff (v, e) -> TypeAndEff (f v, g e) | te -> te) |> ddp  
    let effmap f = function 
      | Effect eff -> Effect (f eff)
      | e -> e
    let effmapi f = function
      | Effect eff -> Effect (StateMap.mapi f eff)
      | e -> e 
    let init_T trace env = trace, TEBot, TEBot, env
    let set_env env (var, ve, fout, _) = var, ve, fout, env
    let table_to_value f (var, ve, fout, env) = 
      if f env then Bot else Table (var, ve, fout, env)
    let get_env_T (_, _, _, env) = env
    let init_fout = TEBot
    let get_name n = match n with
      | SN (_, var) -> var
    let get_trace n = match n with
      | SN (_, var) -> create_empty_trace
    let alpha_rename_fout f fout prevar_trace var_trace = f fout prevar_trace var_trace
    let alpha_rename_T f fr (t:table_t) prevar_trace var_trace :table_t =
      let (z, vi, vo, env) = t in
        (z, f vi prevar_trace var_trace, f vo prevar_trace var_trace, fr env prevar_trace var_trace)
    let join_T f fr g (t1:table_t) (t2:table_t) = let t =
      let (z1, v1i, v1o, env1) = t1 and (z2, v2i, v2o, env2) = t2 in
      let z1', z2' = get_trace_data z1, get_trace_data z2 in
        if String.compare z1' z2' = 0 then (z1, f v1i v2i, f v1o v2o, fr env1 env2) else (*a renaming*)
          let v2o' = g v2o z2' z1' in 
          (z1, f v1i v2i, f v1o v2o', fr env1 env2)
        in t
    let meet_T f fr g (t1:table_t) (t2:table_t) = let t =
      let (z1, v1i, v1o, env1) = t1 and (z2, v2i, v2o, env2) = t2 in
      let z1', z2' = get_trace_data z1, get_trace_data z2 in
        if String.compare z1' z2' = 0 then (z1, f v1i v2i, f v1o v2o, fr env1 env2) else (*a renaming*)
          let v2o' = g v2o z2' z1' in 
          (z1, f v1i v2i, f v1o v2o', fr env1 env2)
        in t
    let leq_T f fr (z1, v1i, v1o, env1) (z2, v2i, v2o, env2) = z1 = z2 && f v1i v2i && f v1o v2o && fr env1 env2
    let eq_T f fr (z1, v1i, v1o, env1) (z2, v2i, v2o, env2) = 
        z1 = z2 && f v1i v2i && f v1o v2o && fr env1 env2
    let forget_T f fr var (t:table_t) = let (z, vi, vo, env) = t in (z, f var vi, f var vo, fr var env)
    let arrow_T f1 f2 fr var (t:table_t) v =
        let (z, vi, vo, env) = t in
        let v' = f1 (get_trace_data z) v in
        let env' = 
          match fr var (Relation env) v with
          | Relation r -> r
          | _ -> raise (Invalid_argument "Should be a relation")
        in
        (z, f2 var vi v, f2 var vo v', env')
    let wid_T f fr g (t1:table_t) (t2:table_t) = let t =
      let (z1, v1i, v1o, env1) = t1 and (z2, v2i, v2o, env2) = t2 in
      let z1', z2' = get_trace_data z1, get_trace_data z2 in
        if String.compare z1' z2' = 0 then (z1, f v1i v2i, f v1o v2o, fr env1 env2) else (*a renaming*)
          let v2o' = g v2o z2' z1' in 
          (z1, f v1i v2i, f v1o v2o', fr env1 env2)
        in t
    (* let equal_T f g (t:table_t) var = 
      let (z, vi, vo) = t in
        let vo' = if (String.compare z var) = 0 then 
          g vo z "z1"
          else vo in
        (z, f vi var, f vo' var) *)
    let replace_fout f fout var_trace x = f fout var_trace x
    let replace_T f fr (t:table_t) var_trace x = let (z, vi, vo, env) = t in
      (z, f vi var_trace x, f vo var_trace x, fr env var_trace x)
    let stren_T f fr (t:table_t) ae = let (z, vi, vo, env) = t in
      (z, f vi ae, f vo ae,
      match fr (Relation env) ae with
      | Relation r -> r
      | _ -> raise (Invalid_argument "Should be a relation"))
    let proj_T f fr g (t:table_t) (acc_vars, qset) vars = let (z, vi, vo, env) = t in
      let var = get_trace_data z in
      let vars_i = var :: vars |> List.append (g var vi) in
      let vars_o =
        let acc_z_vars = 
          List.map (fun q -> List.map (get_input_acc_name var q) acc_vars) qset 
          |> List.flatten
        in
        acc_z_vars @ vars_i
      in
      (z, f vi (acc_vars, qset) vars_i, f vo (acc_vars, qset) vars_o, fr env vars)
    let get_label_snode n = let SN (_, e1) = n in e1
    let construct_vnode env label _ = EN (env, label)
    let construct_enode env label = EN (env, label)
    let construct_snode _ (EN (_, label)) = SN (true, label)
    let construct_fout _ vo = vo
    let construct_table z (vi,vo) env = z, vi, vo, env
    let v_fout _ v = v
    let io_T _ v = match v with
      | TypeAndEff (Table (_, vi, vo, _), _) -> vi, vo
      | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let get_vnode = function
      | EN (env, l) -> env, l, create_singleton_trace_call_loc l
    let get_fout_tails _ = [create_empty_trace]
    let dx_T v = match v with
        | TypeAndEff (Table (z,_,_, _), _) -> z
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let trace_exists _ _ = true
    let get_table_T = function
      | TypeAndEff (Table t, _) -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a],[%s] " f (VarMap.bindings env) l ; Format.fprintf ppf ">@]"
    let print_fout fout ppf f = f ppf fout
    let print_table t ppf f fr = let (z, vi, vo, env) = t in
      Format.fprintf ppf "@[%s" (get_trace_data z); Format.fprintf ppf ": (%a ->@ %a | %a)@]" f vi f vo fr env
    let compare_node n1 n2 = 
      let SN (_, e1) = n1 in
      let SN (_, e2) = n2 in
      comp_trace (create_singleton_trace_call_loc e1) (create_singleton_trace_call_loc e2)
    let prop_table f g fr (t1:table_t) (t2:table_t) = 
      let alpha_rename t1 t2 = let (z1, v1i, v1o, env1) = t1 and (z2, v2i, v2o, env2) = t2 in
        let z1', z2' = get_trace_data z1, get_trace_data z2 in
        if String.compare z1' z2' = 0 then t1, t2
        else (*a renaming*)
          let v2o' = g v2o z2' z1' in
          let v2i' = g v2i z2' z1' in
          (z1, v1i, v1o, env1), (z1, v2i', v2o', env2) 
      in
      let t1', t2' = alpha_rename t1 t2 in
      let (z1, v1i, v1o, env1) = t1' and (z2, v2i, v2o, env2) = t2' in
      let (v1i', v1o'), (v2i', v2o') = f z1 (v1i, v1o) (v2i, v2o) in
      let t1' = (z1, v1i', v1o', env1) and t2' = (z2, v2i', v2o', fr env1 env2) in
      t1', t2'
    let step_func f v m = 
      let z, tl, tr, env = match v with
      | TypeAndEff (Table (z, vi, vo, env), _) -> z, vi, vo, env
      | _ -> raise (Invalid_argument "Should be a table when using step_func") in
      f env z (tl, tr) m
    let get_full_fout fout = [create_empty_trace, fout]
    let get_full_table_T t = let (z, vi, vo, env) = t in
      [z, (vi, vo)], env
    let get_table_by_cs_T _ t = let (_, vi, vo, _) = t in (vi, vo)
    let update_fout cs vo _ = construct_fout cs vo
    let merge_fout f v1 v2 = f v1 v2
    let update_table cs vio (_, _, _, env) = construct_table cs vio env
    let table_isempty _ = false
    let table_mapi _ t = t
    let is_top_fout fout = match fout with
      | TETop -> true
      | _ -> false
    (* let bot_shape_T f fr t = 
      let (z, vi, vo, env) = t in
      (z, f vi, f vo, fr env) *)
    let append_call_trace new_token trace = add_cs_token_to_trace new_token trace 1
    let append_part_trace new_token trace = add_pr_token_to_trace new_token trace 1
    let extend_trace tail trace = add_tail_to_trace tail trace 1
  end

module Sensitive: SemanticsType =
  struct
    type enode_t = env_t * var (*N = E x loc*)
    and vnode_t = env_t * var * call_site (*Nx = E x var x stack*)
    and node_t = EN of enode_t | VN of vnode_t
    and env_t = (node_t * bool) VarMap.t (*E = Var -> N*)
    and call_site = trace_t   (*stack = trace_t * loc*)
    and node_s_t = SEN of (var * call_site) | SVN of (var * call_site) (* call site * label *)
    type value_tt =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t (* [<call_site>: table_t ...]*)
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and eff = EffBot | EffTop | Effect of effect_t
    and fout_t = 
      | Fout of (value_te) TableMap.t
      | FTop
    and value_te = 
      | TEBot  
      | TETop
      | TypeAndEff of value_tt * eff 
    and table_t = ((value_te * fout_t) TableMap.t) * relation_t
    and list_t = (var * var) * (relation_t * value_te)
    and tuple_t = value_te list
   let temap (f, g) te = 
      let ddp = function TypeAndEff (Bot,EffBot) -> TEBot | TypeAndEff (Top,EffTop) -> TETop | te -> te in
      let dp = function TEBot -> TypeAndEff (Bot,EffBot) | TETop -> TypeAndEff (Top,EffTop) | te -> te in 
      match te with 
      | TypeAndEff (v, e) -> TypeAndEff (f v, g e) (* TypeAndEff (apply2 (f, g) ve)  *)      
      |  _ -> dp te |> (function TypeAndEff (v, e) -> TypeAndEff (f v, g e) | te -> te) |> ddp
    let effmap f = function 
      | Effect eff -> Effect (f eff)
      | e -> e
    let effmapi f = function
      | Effect eff -> Effect (StateMap.mapi f eff)
      | e -> e
    let init_fout = Fout TableMap.empty
    let fout_isempty fout = match fout with 
      | FTop -> false    
      | Fout fout -> TableMap.is_empty fout
    let init_T _ env = TableMap.empty, env
    let set_env env (t, _) = t, env
    let table_to_value f (table, env) = 
      if f env then Bot else Table (table, env)
    let get_env_T (_, env) = env
    let get_name n = match n with
      | SEN (var, _) -> var
      | SVN (var, _) -> var
    let get_trace n = match n with
      | SEN (_, cs) -> cs
      | SVN (_, cs) -> cs
    let alpha_rename_fout f fout prevar var = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.map (fun vo_tr -> f vo_tr prevar var) fout)
    let alpha_rename_T f fr ((mt, env):table_t) prevar var = TableMap.map (fun (vi, vo) ->
        f vi prevar var, 
        alpha_rename_fout f vo prevar var
      ) mt, fr env prevar var
    let join_fout f _ fout1 fout2 = match fout1, fout2 with
      | FTop, _ | _, FTop -> FTop
      | Fout fout1, Fout fout2 -> Fout (TableMap.union (fun _ v1o_tr v2o_tr -> 
          Some (f v1o_tr v2o_tr)) fout1 fout2)
    let join_T f fr g (mt1, env1) (mt2, env2) =
      TableMap.union (fun _ (v1i, v1o) (v2i, v2o) -> 
        Some (f v1i v2i, join_fout f g v1o v2o)
      ) mt1 mt2, fr env1 env2
    let meet_fout f _ fout1 fout2 = match fout1, fout2 with
      | FTop, fout | fout, FTop -> fout
      | Fout fout1, Fout fout2 -> Fout (TableMap.union (fun _ v1o_tr v2o_tr -> 
          Some (f v1o_tr v2o_tr)) fout1 fout2)
    let meet_T f fr g (mt1, env1) (mt2, env2) =
        TableMap.merge (fun _ vio1 vio2 -> 
          match vio1, vio2 with
          | None, _ | _, None -> None
          | Some (v1i, v1o), Some (v2i, v2o) -> 
              Some ((f v1i v2i), meet_fout f g v1o v2o)
          ) mt1 mt2, fr env1 env2
    let is_bot_VE = function
      | TEBot -> true
      | TypeAndEff (Bot, EffBot) -> true
      | _ -> false
    let leq_fout f fout1 fout2 = match fout1, fout2 with
      | FTop, FTop -> true
      | FTop, _ -> false
      | _, FTop -> true
      | Fout fout1, Fout fout2 -> TableMap.for_all (fun cs v1o -> 
          TableMap.find_opt cs fout2 |> Opt.map (fun v2o -> f v1o v2o) |>
          Opt.get_or_else (is_bot_VE v1o)) fout1
    let leq_T f fr (mt1, env1) (mt2, env2) =
      TableMap.for_all (fun cs (v1i, v1o) -> 
        TableMap.find_opt cs mt2 |> Opt.map (fun (v2i, v2o) -> f v1i v2i && leq_fout f v1o v2o) |>
        Opt.get_or_else (is_bot_VE v1i && fout_isempty v1o)) mt1 && fr env1 env2
    let eq_fout f fout1 fout2 = match fout1, fout2 with
      | Fout _, FTop -> false
      | FTop, Fout _ -> false
      | FTop, FTop -> true
      | Fout fout1, Fout fout2 -> TableMap.equal (fun v1o v2o -> f v1o v2o) fout1 fout2
    let eq_T f fr (mt1, env1) (mt2, env2) =
        TableMap.equal (fun (v1i, v1o) (v2i, v2o) -> f v1i v2i && eq_fout f v1o v2o) mt1 mt2 && fr env1 env2
    let forget_fout f var fout = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.map (fun vo -> f var vo) fout)
    let forget_T f fr var (mt, env) = TableMap.map (fun (vi, vo) -> 
      f var vi, forget_fout f var vo) mt, fr var env
    let arrow_fout f var fout v = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.mapi (fun _ vo -> f var vo v) fout)
    let arrow_T f1 f2 fr var (mt, env) v = TableMap.mapi (fun cs (vi, vo) -> 
      let z = get_trace_data cs in
      let v' = f1 z v in
      f2 var vi v, arrow_fout f2 var vo v') mt, 
      match fr var (Relation env) v with
      | Relation r -> r
      | _ -> raise (Invalid_argument "Should be a relation")
    let wid_fout f fout1 fout2 = match fout1, fout2 with
      | Fout fout1, Fout fout2 -> Fout (TableMap.union (fun _ v1o v2o -> Some (f v1o v2o)) fout1 fout2)
      | _, _ -> join_fout f f fout1 fout2
    let wid_T f fr _ (mt1, env1) (mt2, env2) =
      TableMap.union (fun _ (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, wid_fout f v1o v2o)) mt1 mt2, fr env1 env2
    (* let equal_fout f fout var = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.map (fun vo -> f vo var) fout)
    let equal_T f g mt var = TableMap.map (fun (vi, vo) -> f vi var, equal_fout f vo var) mt *)
    let replace_fout f fout var x = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.map (fun vo -> f vo var x) fout)
    let replace_T f fr (mt, env) var x = 
      TableMap.map (fun (vi, vo) -> f vi var x, replace_fout f vo var x) mt, fr env var x
    let stren_fout f fout ae = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.map (fun vo -> f vo ae) fout)
    let stren_T f fr (mt, env) ae = 
      TableMap.map (fun (vi, vo) -> f vi ae, stren_fout f vo ae) mt, 
      match fr (Relation env) ae with
      | Relation r -> r
      | _ -> raise (Invalid_argument "Should be a relation")
    let proj_fout f fout (acc_vars, qset) vars = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.mapi (fun _ vo -> f vo (acc_vars, qset) vars) fout)
    (* let rec pr_vars ppf = function
      | [] -> ()
      | [x] -> Format.fprintf ppf "%s" x
      | x :: vars -> Format.fprintf ppf "%s,@ %a" x pr_vars vars *)
    let proj_T f fr g (mt, env) (acc_vars, qset) vars = TableMap.mapi (fun cs (vi, vo) -> 
      let var = get_trace_data cs in
      let vars_i = var :: vars |> List.append (g var vi) in
      let vars_o =
        let acc_z_vars = 
          List.map (fun q -> List.map (get_input_acc_name var q) acc_vars) qset 
          |> List.flatten
        in
        acc_z_vars @ vars_i
      in
      f vi (acc_vars, qset) vars_i, proj_fout f vo (acc_vars, qset) vars_o) mt, fr env vars
    let get_label_snode n = match n with 
      | SEN (x, l) -> "EN: "^x^";"^get_trace_data l
      | SVN (x, cl) -> "VN: "^x^";"^get_trace_data cl
    (* let get_var_env_node = function
    | VN(env, var, l) -> env, var, l
    | EN(_, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let get_en_env_node = function
      | VN(_, _, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | EN(env, l) -> env, l
    let get_var_s_node = function
      | SVN (varx, xl) -> varx, xl
      | SEN(_, l) -> raise (Invalid_argument ("Expected variable node at "^(get_trace_data l)))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | SEN(var, l) -> var, l *)
    let construct_vnode env label call_site = VN (env, label, call_site)
    let construct_enode env label = EN (env, label)
    let construct_snode cs (n:node_t): node_s_t = match n with
    | EN (_, l) -> SEN (l, cs)
    | VN (_, xl, cl) -> SVN (xl, cl)
    let construct_fout cs vo = Fout (TableMap.singleton cs vo)
    let construct_table cs (vi,vo) env = TableMap.singleton cs (vi, vo), env
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let v_fout tr fout = match fout with
      | FTop -> TETop
      | Fout fout -> TableMap.find_opt tr fout |> Opt.get_or_else TEBot
    let io_T cs v = match v with
      | TypeAndEff (Table (t, _), _) -> TableMap.find cs t
      | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let get_fout_tails fout = match fout with
      | FTop -> raise (Invalid_argument "Error function output")
      | Fout fout -> TableMap.bindings fout |> List.map (fun (tr, _) -> tr) 
          |> fun l -> match l with | [] -> [create_empty_trace] | _ -> l
    let dx_T v = match v with
      | TypeAndEff (Table (t, _), _) -> let cs, _ = 
            try TableMap.min_binding t with
            Not_found -> create_empty_trace , (TEBot, init_fout) 
          in cs
      | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let trace_exists trace (table, _) = TableMap.exists (fun tr _ -> comp_trace trace tr = 0) table
    let get_table_T = function
      | TypeAndEff (Table t, _) -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a],[%s] " 
        f (VarMap.bindings env) l; Format.fprintf ppf ">@]"
      | VN (env, l, cs) -> let var = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@],@ " 
        f (VarMap.bindings env); Format.fprintf ppf ",@[%s]" (get_trace_data var); Format.fprintf ppf "%s>@]" l
    (* let compare_node n1 n2 = match n1, n2 with
      | SEN (var1, _), SEN (var2, _) -> comp_loc var1 var2
      | SEN (var1, _), SVN (var2, _) -> comp_loc var1 var2
      | SVN (var1, _), SEN (var2, _) -> comp_loc var1 var2
      | SVN (var1, e1), SVN (var2, e2) -> let comp_res = comp_loc var1 var2 in
          if comp_res = 0 then
            comp_trace e1 e2 else comp_res *)
    let print_fout fout ppf f =
      let rec pr_table ppf fout = let vo = fout in
        Format.fprintf ppf "@[%a@]" f vo
      and print_table_map ppf fout = match fout with
        | FTop -> Format.fprintf ppf "T"
        | Fout fout -> Format.fprintf ppf "< %a >" pr_table_map (TableMap.bindings fout)
      and pr_table_map ppf = function
        | [] -> ()
        | [row] -> Format.fprintf ppf "%a" pr_table_row row
        | row :: rows -> Format.fprintf ppf "%a;@ %a" pr_table_row row pr_table_map rows
      and pr_table_row ppf (cs, fout) = 
        Format.fprintf ppf "@[<2>%s" (get_trace_data cs); Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table fout
      in print_table_map ppf fout
    let print_table (t, env) ppf f fr = 
      let rec pr_table ppf t = let (vi, vo) = t in
        Format.fprintf ppf "@[(%a ->@ " f vi; print_fout vo ppf f; Format.fprintf ppf ")@]"
      and print_table_map ppf mt = 
        Format.fprintf ppf "[ %a |@ %a]" pr_table_map (TableMap.bindings mt) fr env
      and pr_table_map ppf = function
        | [] -> ()
        | [row] -> Format.fprintf ppf "%a" pr_table_row row
        | row :: rows -> Format.fprintf ppf "%a;@ %a" pr_table_row row pr_table_map rows
      and pr_table_row ppf (cs, t) = 
        Format.fprintf ppf "@[<2>%s" (get_trace_data cs); Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table t
      in print_table_map ppf t
    let compare_node n1 n2 = match n1, n2 with
      | SEN (var1, _), SEN (var2, _) -> comp_loc var1 var2
      | SEN (var1, _), SVN (var2, _) -> comp_loc var1 var2
      | SVN (var1, _), SEN (var2, _) -> comp_loc var1 var2
      | SVN (var1, e1), SVN (var2, e2) -> let comp_res = comp_loc var1 var2 in
          if comp_res = 0 then
            comp_trace e1 e2 else comp_res
    let merge_fout f fout1 fout2 =
      match fout1, fout2 with
      | FTop, _ | _, FTop -> FTop, FTop
      | Fout fout1, Fout fout2 ->
        let fout = TableMap.merge (fun _ vo1 vo2 ->
          let vo1, vo2 = 
            match vo1, vo2 with
              | None, Some vo2 -> TEBot, vo2
              | Some vo1, None -> vo1, TEBot
              | Some vo1, Some vo2 -> vo1, vo2
              | _, _ -> TEBot, TEBot
          in
          let vo1', vo2' = f vo1 vo2 in
          Some (vo1', vo2')
          ) fout1 fout2 in
        let fout1' = Fout (TableMap.map (fun (fout1, _) -> fout1) fout) in
        let fout2' = Fout (TableMap.map (fun (_, fout2) -> fout2) fout) in
        fout1', fout2'

    let prop_fout f cs (v1i, fout1) (v2i, fout2) = match fout1, fout2 with
      | FTop, _ | _, FTop -> (v1i, FTop), (v2i, FTop)
      | Fout fout1, Fout fout2 ->
        let (v1i', _), (v2i', _) = f cs (v1i, TEBot) (v2i, TEBot) in
        let fout = TableMap.merge (fun _ vo1 vo2 ->
          let vo1, vo2 = 
            match vo1, vo2 with
              | None, Some vo2 -> TEBot, vo2
              | Some vo1, None -> vo1, TEBot
              | Some vo1, Some vo2 -> vo1, vo2
              | _, _ -> TEBot, TEBot
          in
          let (_, vo1'), (_, vo2') = f cs (v1i, vo1) (v2i, vo2) in
          Some (vo1', vo2')
          ) fout1 fout2 in
        let fout1' = Fout (TableMap.map (fun (fout1, _) -> fout1) fout) in
        let fout2' = Fout (TableMap.map (fun (_, fout2) -> fout2) fout) in
        (v1i', fout1'), (v2i', fout2')
    let prop_table f _ fr (t1, env1) (t2, env2) = 
      let t = TableMap.merge (fun cs vio1 vio2 ->
        match vio1, vio2 with
         | None, Some (v2i, v2o) -> Some ((v2i, init_fout), (v2i, v2o))
         | Some (v1i, v1o), None -> Some ((v1i, v1o), (TEBot, init_fout))
         | Some (v1i, v1o), Some (v2i, v2o) -> 
            let t1', t2' = prop_fout f cs (v1i, v1o) (v2i, v2o) in
            Some (t1', t2')
         | _, _ -> None
        ) t1 t2 in
        let t1' = TableMap.map (fun (vio1, _) -> vio1) t in
        let t2' = 
            let temp_mt2 = TableMap.map (fun (_, vio2) -> vio2) t in
            TableMap.filter (fun _ (v2i, v2o) -> not@@is_bot_VE v2i || not@@fout_isempty v2o) temp_mt2
            (*Q: How to detect node scoping?*)
       in (t1', env1), (t2', fr env1 env2)
    let step_func f v m = let (t, env) = get_table_T v in
      TableMap.fold (f env) t m
    let get_full_fout fout = match fout with
      | FTop -> raise (Invalid_argument("Fout with error"))
      | Fout fout -> 
        if TableMap.is_empty fout then 
          [create_empty_trace, TEBot]
        else
          TableMap.bindings fout
    let get_full_table_T (t, env) = 
      let vals = TableMap.bindings t in (vals, env)
    let get_table_by_cs_T cs (t, _) = TableMap.find cs t
    let update_fout cs vo fout = match fout with
      | FTop -> FTop
      | Fout fout -> Fout (TableMap.add cs vo fout)
    let update_table cs vio (t, env) = TableMap.add cs vio t, env
    let table_isempty (t, _) = TableMap.is_empty t
    let table_mapi f (t, env) = (TableMap.mapi f t, env)
    let is_top_fout fout = match fout with
      | FTop -> true
      | _ -> false
    (* let bot_shape_fout f fout = match fout with
      | FTop -> init_fout
      | Fout fout -> Fout (TableMap.mapi (fun _ vo -> f vo) fout) *)
    (* let bot_shape_T f fr (t, env) = 
      TableMap.mapi (fun cs (vi, vo) -> 
        (f vi, bot_shape_fout f vo)) t, fr env *)
    let append_call_trace new_token trace = add_cs_token_to_trace new_token trace !trace_len
    let append_part_trace new_token trace = add_pr_token_to_trace new_token trace !trace_len
    let extend_trace tail trace = add_tail_to_trace tail trace !trace_len
  end

let parse_sensitive = function
  | 0 -> (module NonSensitive: SemanticsType)
  | _ -> (module Sensitive: SemanticsType)

module SenSemantics = (val (!trace_len |> parse_sensitive))

module TempNodeMap = MakeHash(struct
  type t = SenSemantics.node_s_t
  end)

module NodeMap = struct
  include TempNodeMap
  let find key (m: 'a t): 'a = let e1 = SenSemantics.get_label_snode key in
    try TempNodeMap.find key m
    with Not_found -> raise (Key_not_found (e1^" is not Found in NodeMap"))
end

type exec_map_t = SenSemantics.value_te NodeMap.t
