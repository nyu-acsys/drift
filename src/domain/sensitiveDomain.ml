open AbstractDomain
open Syntax
open EffectAutomataSyntax
open Util
open Config
open TracePartDomain

exception AssertionError of string

let name_of_node lb = ("z" ^ lb)

type relation_t = Int of AbstractValue.t 
  | Bool of AbstractValue.t * AbstractValue.t
  | Unit of unit
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
    fun key v -> Some (f v)
  ) m; m
  let mapi f (m: 'a t) : 'a t = Hashtbl.filter_map_inplace (
    fun key v -> Some (f key v)
  ) m; m
  let find key (m: 'a t): 'a = Hashtbl.find m key
  let bindings (m: 'a t) = Hashtbl.fold (
    fun key v lst -> (key, v) :: lst
  ) m []
  let fold = Hashtbl.fold
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
type acc_t = relation_e VarMap.t
module StateMap = Map.Make(struct type t = state_t let compare (Q q1) (Q q2) = compare q1 q2 end)
type effect_t = acc_t  StateMap.t

module type SemanticsType =
  sig
    type node_t
    type node_s_t
    type env_t = (node_t * bool) VarMap.t
    type table_t
    type call_site
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
    val effmapi: (state_t -> relation_e VarMap.t -> relation_e VarMap.t) -> eff -> eff
    val init_T: trace_t -> table_t
    val alpha_rename_T: (value_te -> string -> string -> value_te) -> table_t -> string -> string -> table_t
    val join_T: (value_te -> value_te -> value_te) -> (value_te -> string -> string -> value_te) -> table_t -> table_t -> table_t
    val meet_T: (value_te -> value_te -> value_te) -> (value_te -> string -> string -> value_te) -> table_t -> table_t -> table_t
    val leq_T: (value_te -> value_te -> bool) -> table_t -> table_t -> bool
    val eq_T: (value_te -> value_te -> bool) -> table_t -> table_t -> bool
    val forget_T: (var -> value_te -> value_te) -> var -> table_t -> table_t
    val arrow_T: (var -> value_tt -> value_tt) -> (var -> value_te -> value_tt -> value_te) -> var -> table_t -> value_tt -> table_t
    val wid_T: (value_te -> value_te -> value_te) -> (value_te -> string -> string -> value_te) -> table_t -> table_t -> table_t
    val equal_T: (value_te -> var -> value_te) -> (value_te -> string -> string -> value_te) -> table_t -> var -> table_t
    val replace_T: (value_te -> var -> var -> value_te) -> table_t -> var -> var -> table_t
    val stren_T: (value_te -> value_tt -> value_te) -> table_t -> value_tt -> table_t
    val proj_T: (value_te -> string list -> value_te) -> (value_te -> string list) -> table_t -> string list -> table_t
    val bot_shape_T: (value_te -> value_te) -> table_t -> table_t
    val get_label_snode: node_s_t -> string
    val construct_vnode: env_t -> var -> trace_t -> node_t
    val construct_enode: env_t -> var -> node_t
    val construct_snode: trace_t -> node_t -> node_s_t
    val construct_table: trace_t -> value_te * value_te -> table_t
    val io_T: trace_t -> value_te -> value_te * value_te
    val get_vnode: node_t -> env_t * var * trace_t
    val dx_T: value_te -> trace_t
    val get_table_T: value_te -> table_t
    val print_node: node_t -> Format.formatter -> (Format.formatter -> (string * (node_t * bool)) list -> unit) -> unit
    val print_table: table_t -> Format.formatter -> (Format.formatter -> value_te -> unit) -> unit
    val compare_node: node_s_t -> node_s_t -> int
    val prop_table: (trace_t -> value_te * value_te -> value_te * value_te -> (value_te * value_te) * (value_te * value_te)) -> (value_te -> var -> var -> value_te) -> table_t -> table_t -> table_t * table_t
    val step_func: (trace_t -> value_te * value_te -> 'a -> 'a) -> value_te -> 'a -> 'a
    val get_full_table_T: table_t -> trace_t * (value_te * value_te)
    val get_table_by_cs_T: trace_t -> table_t -> (value_te * value_te)
    val update_table: trace_t -> value_te * value_te -> table_t -> table_t
    val table_isempty: table_t -> bool
    val table_mapi: (trace_t -> value_te * value_te -> value_te * value_te) -> table_t -> table_t
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
    and table_t = var * value_te * value_te
    and list_t = (var * var) * (relation_t * value_te)
    and tuple_t = value_te list
    type call_site = None (* Not used *)
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
    let init_T trace = get_trace_data trace, TEBot, TEBot
    let alpha_rename_T f (t:table_t) prevar_trace var_trace :table_t =
      let (z, vi, vo) = t in
        (z, f vi prevar_trace var_trace, f vo prevar_trace var_trace)
    let join_T f g (t1:table_t) (t2:table_t) = let t =
      let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if String.compare z1 z2 = 0 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let meet_T f g (t1:table_t) (t2:table_t) = let t =
      let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if String.compare z1 z2 = 0 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let leq_T f (z1, v1i, v1o) (z2, v2i, v2o) = z1 = z2 && f v1i v2i && f v1o v2o
    let eq_T f (z1, v1i, v1o) (z2, v2i, v2o) = 
        z1 = z2 && f v1i v2i && f v1o v2o
    let forget_T f var (t:table_t) = let (z, vi, vo) = t in (z, f var vi, f var vo)
    let arrow_T f1 f2 var (t:table_t) v =
        let (z, vi, vo) = t in
        let v' = f1 z v in
        (z, f2 var vi v, f2 var vo v')
    let wid_T f g (t1:table_t) (t2:table_t) = let t =
      let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if String.compare z1 z2 = 0 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let equal_T f g (t:table_t) var = 
      let (z, vi, vo) = t in
        let vo' = if (String.compare z var) = 0 then 
          g vo z "z1"
          else vo in
        (z, f vi var, f vo' var)
    let replace_T f (t:table_t) var_trace x = let (z, vi, vo) = t in
      (z, f vi var_trace x, f vo var_trace x)
    let stren_T f (t:table_t) ae = let (z, vi, vo) = t in
      (z, f vi ae, f vo ae)
    let proj_T f g (t:table_t) vars = let (z, vi, vo) = t in
      let vars_o = 
        let vars = z :: vars in
        List.append vars (g vi)
      in
      (z, f vi vars, f vo vars_o)
    let get_label_snode n = let SN (_, e1) = n in e1
    let construct_vnode env label _ = EN (env, label)
    let construct_enode env label = EN (env, label)
    let construct_snode trace (EN (_, label)) = SN (true, label)
    let construct_table z (vi,vo) = get_trace_data z, vi, vo
    let io_T _ v = match v with
      | TypeAndEff (Table (_, vi, vo), _) -> vi, vo
      | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let get_vnode = function
      | EN (env, l) -> env, l, create_singleton_trace_call_loc l
    let dx_T v = match v with
        | TypeAndEff (Table (z,_,_), _) -> create_singleton_trace_call_loc z
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | TypeAndEff (Table t, _) -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a],[%s] " f (VarMap.bindings env) l ; Format.fprintf ppf ">@]"
    let print_table t ppf f = let (z, vi, vo) = t in
      Format.fprintf ppf "@[%s" z; Format.fprintf ppf ": (%a ->@ %a)@]" f vi f vo
    let compare_node n1 n2 = 
      let SN (_, e1) = n1 in
      let SN (_, e2) = n2 in
      comp_trace (create_singleton_trace_call_loc e1) (create_singleton_trace_call_loc e2)
    let prop_table f g (t1:table_t) (t2:table_t) = 
      let alpha_rename t1 t2 = let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then t1, t2
        else (*a renaming*)
          let v2o' = g v2o z2 z1 in
          (z1, v1i, v1o), (z1, v2i, v2o') 
      in
      let t1', t2' = alpha_rename t1 t2 in
      let (z1, v1i, v1o) = t1' and (z2, v2i, v2o) = t2' in
      let z_tr1 = create_singleton_trace_call_loc z1 in
      let (v1i', v1o'), (v2i', v2o') = f z_tr1 (v1i, v1o) (v2i, v2o) in
      let t1' = (z1, v1i', v1o') and t2' = (z2, v2i', v2o') in
      t1', t2'
    let step_func f v m = 
      let z, tl, tr = match v with
      | TypeAndEff (Table (z, vi, vo), _) -> z, vi, vo
      | _ -> raise (Invalid_argument "Should be a table when using io_T") in
      let z_tr = create_singleton_trace_call_loc z in
      f z_tr (tl, tr) m
    let get_full_table_T t = let (z, vi, vo) = t in
      create_singleton_trace_call_loc z, (vi, vo)
    let get_table_by_cs_T cs t = let (z, vi, vo) = t in (vi, vo)
    let update_table cs vio t = construct_table cs vio
    let table_isempty t = false
    let table_mapi f t = t
    let bot_shape_T f t = 
      let (z, vi, vo) = t in
      (z, f vi, f vo)
    let append_call_trace new_token trace = add_cs_token_to_trace new_token trace 1
    let append_part_trace new_token trace = add_pr_token_to_trace new_token trace 1
    let extend_trace tail trace = add_tail_to_trace tail trace 1
  end

module OneSensitive: SemanticsType =
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
    and value_te = 
      | TEBot  
      | TETop
      | TypeAndEff of value_tt * eff 
    and table_t = (value_te * value_te) TableMap.t
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
    let init_T var = TableMap.empty
    let alpha_rename_T f (mt:table_t) prevar var = TableMap.map (fun (vi, vo) ->
      f vi prevar var, f vo prevar var) mt
    let join_T f g mt1 mt2 =
      TableMap.union (fun cs (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, f v1o v2o)) mt1 mt2
    let meet_T f g mt1 mt2 =
        TableMap.merge (fun cs vio1 vio2 -> 
          match vio1, vio2 with
          | None, _ | _, None -> None
          | Some (v1i, v1o), Some (v2i, v2o) -> Some ((f v1i v2i), (f v1o v2o))
          ) mt1 mt2
    let leq_T f mt1 mt2 =
      TableMap.for_all (fun cs (v1i, v1o) -> 
        TableMap.find_opt cs mt2 |> Opt.map (fun (v2i, v2o) -> f v1i v2i && f v1o v2o) |>
        Opt.get_or_else (v1i = TEBot && v1o = TEBot)) mt1
    let eq_T f mt1 mt2 =
        TableMap.equal (fun (v1i, v1o) (v2i, v2o) -> f v1i v2i && f v1o v2o) mt1 mt2
    let forget_T f var mt = TableMap.map (fun (vi, vo) -> 
      f var vi, f var vo) mt
    let arrow_T f1 f2 var mt v = TableMap.mapi (fun cs (vi, vo) -> 
      let z = get_trace_data cs in
      let v' = f1 z v in
      f2 var vi v, f2 var vo v') mt
    let wid_T f g mt1 mt2 =
      TableMap.union (fun cs (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, f v1o v2o)) mt1 mt2
    let equal_T f g mt var = TableMap.map (fun (vi, vo) -> f vi var, f vo var) mt
    let replace_T f mt var x = TableMap.map (fun (vi, vo) -> f vi var x, f vo var x) mt
    let stren_T f mt ae = TableMap.map (fun (vi, vo) -> f vi ae, f vo ae) mt
    let proj_T f g mt vars = TableMap.mapi (fun cs (vi, vo) -> 
      let var = get_trace_data cs in
      let vars_o = 
        let vars = var :: vars in
        List.append vars (g vi)
      in
      f vi vars_o, f vo vars_o) mt
    let get_label_snode n = match n with 
      | SEN (x, l) -> "EN: "^x^";"^get_trace_data l
      | SVN (x, cl) -> "VN: "^x^";"^get_trace_data cl
    let get_var_env_node = function
    | VN(env, var, l) -> env, var, l
    | EN(env, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let get_en_env_node = function
      | VN(env, var, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | EN(env, l) -> env, l
    let get_var_s_node = function
      | SVN (varx, xl) -> varx, xl
      | SEN(_, l) -> raise (Invalid_argument ("Expected variable node at "^(get_trace_data l)))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | SEN(var, l) -> var, l
    let construct_vnode env label call_site = VN (env, label, call_site)
    let construct_enode env label = EN (env, label)
    let construct_snode cs (n:node_t): node_s_t = match n with
    | EN (env, l) -> SEN (l, cs)
    | VN (env, xl, cl) -> SVN (xl, cl)
    let construct_table cs (vi,vo) = TableMap.singleton cs (vi, vo)
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let io_T cs v = match v with
        | TypeAndEff (Table t, _) -> TableMap.find cs t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let dx_T v = match v with
        | TypeAndEff (Table t, _) -> let cs, _ = 
              try TableMap.min_binding t with
              Not_found -> create_empty_trace , (TEBot,TEBot) 
            in cs
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | TypeAndEff (Table t, _) -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a],[%s] " 
        f (VarMap.bindings env) l; Format.fprintf ppf ">@]"
      | VN (env, l, cs) -> let var = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@],@ " 
        f (VarMap.bindings env); Format.fprintf ppf ",@ "; print_trace ppf var; Format.fprintf ppf "%s>@]" l
    let print_table t ppf f = 
      let rec pr_table ppf t = let (vi, vo) = t in
        Format.fprintf ppf "@[(%a ->@ %a)@]" f vi f vo
      and print_table_map ppf mt = 
        Format.fprintf ppf "[ %a ]" pr_table_map (TableMap.bindings mt)
      and pr_table_map ppf = function
        | [] -> ()
        | [row] -> Format.fprintf ppf "%a" pr_table_row row
        | row :: rows -> Format.fprintf ppf "%a;@ %a" pr_table_row row pr_table_map rows
      and pr_table_row ppf (cs, t) = 
        Format.fprintf ppf "@[<2>%s" (get_trace_data cs); Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table t
      in print_table_map ppf t
    let compare_node n1 n2 = match n1, n2 with
      | SEN (var1, e1), SEN (var2, e2) -> comp_loc var1 var2
      | SEN (var1, e1), SVN (var2, e2) -> comp_loc var1 var2
      | SVN (var1, e1), SEN (var2, e2) -> comp_loc var1 var2
      | SVN (var1, e1), SVN (var2, e2) -> let comp_res = comp_loc var1 var2 in
          if comp_res = 0 then
            comp_trace e1 e2 else comp_res
              
    let prop_table f g t1 t2 = 
      let t = TableMap.merge (fun cs vio1 vio2 ->
        match vio1, vio2 with
         | None, Some (v2i, v2o) -> Some ((v2i, TEBot), (v2i, v2o))
         | Some (v1i, v1o), None -> Some ((v1i, v1o), (TEBot, TEBot))
         | Some (v1i, v1o), Some (v2i, v2o) -> 
            let t1', t2' = f cs (v1i, v1o) (v2i, v2o) in
            Some (t1', t2')
         | _, _ -> None
        ) t1 t2 in
        let t1' = TableMap.map (fun (vio1, _) -> vio1) t in
        let t2' = 
            let temp_mt2 = TableMap.map (fun (_, vio2) -> vio2) t in
            TableMap.filter (fun cs (v2i, v2o) -> v2i <> TEBot || v2o <> TEBot) temp_mt2
            (*Q: How to detect node scoping?*)
       in t1', t2'
    let step_func f v m = let t = get_table_T v in
      TableMap.fold f t m
    let get_full_table_T t = TableMap.min_binding t
    let get_table_by_cs_T cs t = TableMap.find cs t
    let update_table cs vio t = TableMap.add cs vio t
    let table_isempty t = TableMap.is_empty t
    let table_mapi f t = TableMap.mapi f t
    let bot_shape_T f t = 
      TableMap.mapi (fun cs (vi, vo) -> 
        (f vi, f vo)) t
    let append_call_trace new_token trace = add_cs_token_to_trace new_token trace 1
    let append_part_trace new_token trace = add_pr_token_to_trace new_token trace 1
    let extend_trace tail trace = add_tail_to_trace tail trace !trace_len
  end

module NSensitive: SemanticsType =
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
    and value_te = 
      | TEBot  
      | TETop
      | TypeAndEff of value_tt * eff
    and table_t = (value_te * value_te) TableMap.t
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
    let init_T var = TableMap.empty
    let alpha_rename_T f (mt:table_t) prevar var = TableMap.map (fun (vi, vo) ->
      f vi prevar var, f vo prevar var) mt
    let join_T f g mt1 mt2 =
      TableMap.union (fun cs (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, f v1o v2o)) mt1 mt2
    let meet_T f g mt1 mt2 =
        TableMap.merge (fun cs vio1 vio2 -> 
          match vio1, vio2 with
          | None, _ | _, None -> None
          | Some (v1i, v1o), Some (v2i, v2o) -> Some ((f v1i v2i), (f v1o v2o))
          ) mt1 mt2
    let leq_T f mt1 mt2 =
      TableMap.for_all (fun cs (v1i, v1o) -> 
        TableMap.find_opt cs mt2 |> Opt.map (fun (v2i, v2o) -> f v1i v2i && f v1o v2o) |>
        Opt.get_or_else (v1i = TEBot && v1o = TEBot)) mt1
    let eq_T f mt1 mt2 =
        TableMap.equal (fun (v1i, v1o) (v2i, v2o) -> f v1i v2i && f v1o v2o) mt1 mt2
    let forget_T f var mt = TableMap.map (fun (vi, vo) -> 
      f var vi, f var vo) mt
    let arrow_T f1 f2 var mt v = TableMap.mapi (fun cs (vi, vo) -> 
      let z = get_trace_data cs in
      let v' = f1 z v in
      f2 var vi v, f2 var vo v') mt
    let wid_T f g mt1 mt2 =
      TableMap.union (fun cs (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, f v1o v2o)) mt1 mt2
    let equal_T f g mt var = TableMap.map (fun (vi, vo) -> f vi var, f vo var) mt
    let replace_T f mt var x = TableMap.map (fun (vi, vo) -> f vi var x, f vo var x) mt
    let stren_T f mt ae = TableMap.map (fun (vi, vo) -> f vi ae, f vo ae) mt
    let proj_T f g mt vars = TableMap.mapi (fun cs (vi, vo) -> 
      let var = get_trace_data cs in
      let vars_o = 
        let vars = var :: vars in
        List.append vars (g vi)
      in
      f vi vars, f vo vars_o) mt
    let get_label_snode n = match n with 
      | SEN (x, l) -> "EN: "^x^";"^get_trace_data l
      | SVN (x, cl) -> "VN: "^x^";"^get_trace_data cl
    let get_var_env_node = function
    | VN(env, var, l) -> env, var, l
    | EN(env, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let get_en_env_node = function
      | VN(env, var, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | EN(env, l) -> env, l
    let get_var_s_node = function
      | SVN (varx, xl) -> varx, xl
      | SEN(_, l) -> raise (Invalid_argument ("Expected variable node at "^(get_trace_data l)))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | SEN(var, l) -> var, l
    let construct_vnode env label call_site = VN (env, label, call_site)
    let construct_enode env label = EN (env, label)
    let construct_snode cs (n:node_t): node_s_t = match n with
    | EN (env, l) -> SEN (l, cs)
    | VN (env, xl, cl) -> SVN (xl, cl)
    let construct_table cs (vi,vo) = TableMap.singleton cs (vi, vo)
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let io_T cs v = match v with
        | TypeAndEff (Table t, _) -> TableMap.find cs t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let dx_T v = match v with
        | TypeAndEff (Table t, _) -> let cs, _ = 
              try TableMap.min_binding t with
              Not_found -> create_empty_trace , (TEBot,TEBot) 
            in cs
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | TypeAndEff (Table t, _) -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a],[%s] " 
        f (VarMap.bindings env) l; Format.fprintf ppf ">@]"
      | VN (env, l, cs) -> let var = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@],@ " 
        f (VarMap.bindings env); Format.fprintf ppf ",@ "; print_trace ppf var; Format.fprintf ppf "%s>@]" l
    let print_table t ppf f = 
      let rec pr_table ppf t = let (vi, vo) = t in
        Format.fprintf ppf "@[(%a ->@ %a)@]" f vi f vo
      and print_table_map ppf mt = 
        Format.fprintf ppf "[ %a ]" pr_table_map (TableMap.bindings mt)
      and pr_table_map ppf = function
        | [] -> ()
        | [row] -> Format.fprintf ppf "%a" pr_table_row row
        | row :: rows -> Format.fprintf ppf "%a;@ %a" pr_table_row row pr_table_map rows
      and pr_table_row ppf (cs, t) = 
        Format.fprintf ppf "@[<2>%s" (get_trace_data cs); Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table t
      in print_table_map ppf t
    let compare_node n1 n2 = match n1, n2 with
      | SEN (var1, e1), SEN (var2, e2) -> comp_loc var1 var2
      | SEN (var1, e1), SVN (var2, e2) -> comp_loc var1 var2
      | SVN (var1, e1), SEN (var2, e2) -> comp_loc var1 var2
      | SVN (var1, e1), SVN (var2, e2) -> let comp_res = comp_loc var1 var2 in
          if comp_res = 0 then
            comp_trace e1 e2 else comp_res
              
    let prop_table f g t1 t2 = 
      let t = TableMap.merge (fun cs vio1 vio2 ->
        match vio1, vio2 with
         | None, Some (v2i, v2o) -> Some ((v2i, TEBot), (v2i, v2o))
         | Some (v1i, v1o), None -> Some ((v1i, v1o), (TEBot, TEBot))
         | Some (v1i, v1o), Some (v2i, v2o) -> 
            let t1', t2' = f cs (v1i, v1o) (v2i, v2o) in
            Some (t1', t2')
         | _, _ -> None
        ) t1 t2 in
      let t1' = TableMap.map (fun (vio1, _) -> vio1) t in
      let t2' = 
          let temp_mt2 = TableMap.map (fun (_, vio2) -> vio2) t in
          TableMap.filter (fun cs (v2i, v2o) -> v2i <> TEBot || v2o <> TEBot) temp_mt2
          (*Q: How to detect node scoping?*)
      in t1', t2'
    let step_func f v m = let t = get_table_T v in
      TableMap.fold f t m
    let get_full_table_T t = TableMap.min_binding t
    let get_table_by_cs_T cs t = TableMap.find cs t
    let update_table cs vio t = TableMap.add cs vio t
    let table_isempty t = TableMap.is_empty t
    let table_mapi f t = TableMap.mapi f t
    let bot_shape_T f t = 
      TableMap.mapi (fun cs (vi, vo) -> 
        (f vi, f vo)) t
    let append_call_trace new_token trace = add_cs_token_to_trace new_token trace !trace_len
    let append_part_trace new_token trace = add_pr_token_to_trace new_token trace !trace_len
    let extend_trace tail trace = add_tail_to_trace tail trace !trace_len
  end

(*module NSensitive: SemanticsType =
  struct
    type enode_t = env_t * call_site (*N = E x loc*)
    and vnode_t = env_t * var * call_site (*Nx = E x var x stack*)
    and node_t = EN of enode_t | VN of vnode_t
    and env_t = (node_t * bool) VarMap.t (*E = Var -> N*)
    and call_site = trace_t list   (*stack = trace_t * loc*)
    and node_s_t = SEN of (var * call_site) | SVN of (var * call_site) (* call site * label *)
    type value_tt =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t (* [<call_site>: table_t ...]*)
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and table_t = (value_te * value_te) TableMap.t
    and list_t = (var * var) * (relation_t * value_te)
    and tuple_t = value_te list
    let init_T _ = TableMap.empty
    let alpha_rename_T f (mt:table_t) prevar var = TableMap.map (fun (vi, vo) ->
      f vi prevar var, f vo prevar var) mt

    let get_traces_from_table table = List.map (fun ((var, trace), _) -> var, trace) table
    let trace_to_var_trace (var, trace) = None_Loc_Token var :: trace
    let var_trace_to_trace trace = List.hd trace |> get_token_loc, List.tl trace
    
    let join_T f g mt1 mt2 = 
      let rec add_traces_to_table bindings trace_trees table_add_function table = match bindings with
        | [] -> table
        | ((var, trace), (vi, vo)) :: tail_bindings ->
          let var_trace = trace_to_var_trace (var, trace) in
          let tree = None_Loc_Token var |> find_head_in_tree_list trace_trees in
          let common_trace = find_trace_tree_longest_common_trace var_trace tree |> var_trace_to_trace |> snd in 
          TableMap.update (var, common_trace) (table_add_function (vi, vo)) table |>
            add_traces_to_table tail_bindings trace_trees table_add_function 
      in
      let table_add_function = (fun (vi, vo) v -> match v with | None -> Some (vi, vo) | Some (v1i, v1o) -> Some (f vi v1i, f vo v1o)) in
      let var_traces1 = TableMap.bindings mt1 |> get_traces_from_table |> List.map trace_to_var_trace in
      let var_traces2 = TableMap.bindings mt2 |> get_traces_from_table |> List.map trace_to_var_trace in
      let joined_var_traces = join_traces var_traces1 var_traces2 in
      let var_trace_trees = sort_trees joined_var_traces in
      add_traces_to_table (TableMap.bindings mt1) var_trace_trees table_add_function TableMap.empty
        |> add_traces_to_table (TableMap.bindings mt2) var_trace_trees table_add_function
    
    let meet_T f g mt1 mt2 = 
      let rec add_traces_to_table var_traces1 var_traces2 var_trace_trees table_add_function mt1 mt2 table = 
        match var_traces1, var_traces2 with
        | [], [] -> table
        | _, [] -> add_traces_to_table [] var_traces1 var_trace_trees table_add_function mt2 mt1 table
        | [], var_trace2 :: tail2 -> 
            let (var2, trace2) = var_trace_to_trace var_trace2 in
            let (vi2, vo2) = TableMap.find (var2, trace2) mt2 in
            let tree = List.hd var_trace_trees in
            if tree_contains_trace var_trace2 tree then
              TableMap.update (var2, trace2) (table_add_function (vi2, vo2)) table |>
                add_traces_to_table [] tail2 var_trace_trees table_add_function mt1 mt2
            else 
              if List.length var_trace_trees >= 2 then 
                let next_tree = List.tl var_trace_trees |> List.hd in
                if tree_contains_trace var_trace2 next_tree then
                  add_traces_to_table var_traces1 var_traces2 (List.tl var_trace_trees) table_add_function mt1 mt2 table
                else
                  table
              else
                table
        | var_trace1 :: tail1, var_trace2 :: tail2 -> 
            if comp_trace var_trace1 var_trace2 > 0 then (* P1: smallest trace added first *)
              add_traces_to_table var_traces2 var_traces1 var_trace_trees table_add_function mt2 mt1 table
            else
              let (var1, trace1) = var_trace_to_trace var_trace1 in
              let (vi1, vo1) = TableMap.find (var1, trace1) mt1 in
              let tree = List.hd var_trace_trees in
              if tree_contains_trace var_trace1 tree then
                if is_maximal_trace var_trace1 tree then (* maximal traces added first *)
                  TableMap.update (var1, trace1) (table_add_function (vi1, vo1)) table |>
                    add_traces_to_table tail1 var_traces2 var_trace_trees table_add_function mt1 mt2
                else
                  if tree_contains_trace var_trace2 tree then (* if trace1 is not maximal, trace2 can't be maximal as it violates P1 *)
                    raise (AssertionError "meet_T.add_traces_to_table: trace2 smaller than trace 1")
                  else (* trace1 is not maximal and has to be added now *)
                    let traces = find_trace_tree_super_traces [] var_trace1 tree |> List.map var_trace_to_trace in
                    List.fold_left (fun table trace -> TableMap.update (var1, trace1) (table_add_function (vi1, vo1)) table) table traces |>
                      add_traces_to_table tail1 var_traces2 var_trace_trees table_add_function mt1 mt2
              else
                if tree_contains_trace var_trace2 tree then 
                (* violates P1 as current tree is smallest and a tree is only changed when "a" trace 1 (i.e. the smallest one at the time) is in a tree.  *)
                  raise (AssertionError "meet_T.add_traces_to_table: trace 1 not in tree, but trace 2 is")
                else 
                  if List.length var_trace_trees >= 2 then 
                    let next_tree = List.tl var_trace_trees |> List.hd in
                    if tree_contains_trace var_trace1 next_tree then
                      add_traces_to_table var_traces1 var_traces2 (List.tl var_trace_trees) table_add_function mt1 mt2 table
                    else
                        add_traces_to_table tail1 var_traces2 var_trace_trees table_add_function mt1 mt2 table
                  else
                    table
        
      in
      let var_traces1 = TableMap.bindings mt1 |> get_traces_from_table |> List.map trace_to_var_trace |> sort_traces in
      let var_traces2 = TableMap.bindings mt2 |> get_traces_from_table |> List.map trace_to_var_trace |> sort_traces in
      let met_var_traces = meet_traces var_traces1 var_traces2 true in (* assumes that this discards non-matches *)
      let var_trace_trees = sort_trees met_var_traces in
      add_traces_to_table var_traces1 var_traces2 var_trace_trees 
          (fun (vi, vo) v -> match v with | None -> Some (vi, vo) | Some (v1i, v1o) ->  Some ((f v1i vi), (f v1o vo))) 
          mt1 mt2 TableMap.empty 

    let leq_T f mt1 mt2 = 
      let leq_T_traces var_trace1 var_trace2 mt1 mt2 = 
        let (var1, trace1), (var2, trace2) = var_trace_to_trace var_trace1, var_trace_to_trace var_trace2 in
        let (v1i, v1o), (v2i, v2o) = TableMap.find (var1, trace1) mt1, TableMap.find (var2, trace2) mt2 in
        f v1i v2i && f v1o v2o
      in

      let var_traces1 = TableMap.bindings mt1 |> get_traces_from_table |> List.map trace_to_var_trace |> sort_traces in
      let var_traces2 = TableMap.bindings mt2 |> get_traces_from_table |> List.map trace_to_var_trace |> sort_traces in
      let var_trees2 = collect_traces var_traces2 |> sort_trees in
      List.for_all (fun var_trace  -> 
        let var_tree = List.hd var_trace |> find_head_in_tree_list var_trees2 in
        try
          let super_var_traces = find_trace_tree_super_traces [] var_trace var_tree in
          List.for_all (fun super_var_trace -> 
            leq_T_traces var_trace super_var_trace mt1 mt2
          ) super_var_traces
        with 
          | Not_found -> TableMap.find (var_trace_to_trace var_trace) mt1 = (Bot, Bot)
          | Trace_larger_than_tree -> 
              let common_trace = find_trace_tree_longest_common_trace var_trace var_tree in
              if is_maximal_trace common_trace var_tree then
                leq_T_traces var_trace common_trace mt1 mt2
              else
                TableMap.find (var_trace_to_trace var_trace) mt1 = (Bot, Bot)
      ) var_traces1

    let eq_T f mt1 mt2 =
        TableMap.equal (fun (v1i, v1o) (v2i, v2o) -> f v1i v2i && f v1o v2o) mt1 mt2

    let forget_T f var mt = TableMap.map (fun (vi, vo) -> 
      f var vi, f var vo) mt

    let arrow_T f1 f2 var mt v = TableMap.mapi (fun cs (vi, vo) -> 
      let z, _ = cs in
      let v' = f1 z v in
      f2 var vi v, f2 var vo v') mt
    
    let wid_T f g mt1 mt2 =
      join_T f g mt1 mt2

    let equal_T f g mt var = TableMap.map (fun (vi, vo) -> f vi var, f vo var) mt
    let replace_T f mt var x = TableMap.map (fun (vi, vo) -> f vi var x, f vo var x) mt
    let stren_T f mt ae = TableMap.map (fun (vi, vo) -> f vi ae, f vo ae) mt
    
    let proj_T f g mt vars = TableMap.mapi (fun cs (vi, vo) -> 
      let var, _ = cs in
      let vars_o = 
        let vars = var :: vars in
        List.append vars (g vi)
      in
      f vi vars, f vo vars_o) mt
    let get_label_snode n = match n with 
      | SEN (v, l) -> v^" "^(get_trace_data l)
      | SVN (xl, cl) -> xl^" "^(get_trace_data cl)
    let get_var_env_node = function
    | VN(env, var, l) -> env, var, l
    | EN(env, l) -> raise (Invalid_argument ("Expected variable node at "^(get_trace_data l)))
    let get_en_env_node = function
      | VN(env, var, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | EN(env, l) -> env, l
    let get_var_s_node = function
      | SVN (varx, xl) -> varx, xl
      | SEN(_, l) -> raise (Invalid_argument ("Expected variable node at "^(get_trace_data l)))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^(get_trace_data l)))
      | SEN(var, l) -> var, l
    let construct_vnode env label call_site = VN (env, label, call_site)
    let construct_enode env label = EN (env, label)
    let construct_snode x (n:node_t): node_s_t = match n with
    | EN (env, l) -> if VarMap.is_empty env || x = "" then SEN (x, l) else
      let _, va, _ = 
        let n, _ = VarMap.find x env in
        get_var_env_node n in
      SEN (va, l)
    | VN (env, xl, cl) -> SVN (xl, cl)
    let construct_table cs (vi,vo) = TableMap.singleton cs (vi, vo)
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l) -> raise (Invalid_argument ("Expected variable node at "^(get_trace_data l)))
    let io_T cs v = match v with
        | Table t -> TableMap.find cs t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let dx_T v = match v with
        | Table t -> let cs, _ = try TableMap.min_binding t with
          Not_found -> ("",create_singleton_trace_loc "") , (Bot,Bot) in cs
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | Table t -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><@[<1>[%a]@], " 
        f (VarMap.bindings env); print_trace ppf l; Format.fprintf ppf "]"
      | VN (env, l, cs) -> let var = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@], " 
        f (VarMap.bindings env); print_trace ppf var; Format.fprintf ppf "%s" l; Format.fprintf ppf "]"
    let print_table t ppf f = 
      let rec pr_table ppf t = let (vi, vo) = t in
        Format.fprintf ppf "@[(%a ->@ %a)@]" f vi f vo
      and print_table_map ppf mt = 
        Format.fprintf ppf "[ %a ]" pr_table_map (TableMap.bindings mt)
      and pr_table_map ppf = function
        | [] -> ()
        | [row] -> Format.fprintf ppf "%a" pr_table_row row
        | row :: rows -> Format.fprintf ppf "%a;@ %a" pr_table_row row pr_table_map rows
      and pr_table_row ppf (cs, t) = 
        let var, l = cs in
        Format.fprintf ppf "@[<2>%s" var; Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table t
      in print_table_map ppf t
    let compare_node comp n1 n2 = 
      let var1, var2, e1, e2 = match n1, n2 with
        | SEN (var1, e1), SEN (var2, e2) -> var1, var2, e1, e2
        | SEN (var1, e1), SVN (var2, e2) -> var1, var2, e1, e2
        | SVN (var1, e1), SEN (var2, e2) -> var1, var2, e1, e2
        | SVN (var1, e1), SVN (var2, e2) -> var1, var2, e1, e2
      in
      if comp e1 e2 = 0 then
        String.compare var1 var2 else comp e1 e2
    
    (* todo *)
    let prop_table f g t1 t2 = 
      let t = TableMap.merge (fun cs vio1 vio2 ->
        match vio1, vio2 with
         | None, Some (v2i, v2o) -> Some ((v2i, Bot), (v2i, v2o))
         | Some (v1i, v1o), None -> Some ((v1i, v1o), (Bot, Bot))
         | Some (v1i, v1o), Some (v2i, v2o) -> 
            let t1', t2' = f cs (v1i, v1o) (v2i, v2o) in
            Some (t1', t2')
         | _, _ -> None
        ) t1 t2 in
        let t1' = TableMap.map (fun (vio1, _) -> vio1) t in
        let t2' = 
            let temp_mt2 = TableMap.map (fun (_, vio2) -> vio2) t in
            TableMap.filter (fun cs (v2i, v2o) -> v2i <> Bot || v2o <> Bot) temp_mt2
            (*Q: How to detect node scoping?*)
       in t1', t2'
    let step_func f v m = let t = get_table_T v in
      TableMap.fold f t m
    let get_full_table_T t = TableMap.min_binding t
    let get_table_by_cs_T cs t = TableMap.find cs t
    let update_table cs vio t = TableMap.add cs vio t
    let table_isempty t = TableMap.is_empty t
    let table_mapi f t = TableMap.mapi f t
    let bot_shape_T f t = 
      TableMap.mapi (fun cs (vi, vo) -> 
        (f vi, f vo)) t
end*)

let parse_sensitive = function
  | 0 -> (module NonSensitive: SemanticsType)
  | 1 -> (module OneSensitive: SemanticsType)
  | _ -> (module NSensitive: SemanticsType)

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
