open AbstractDomain
open DriftSyntax
open Util
open Config
open TracePartDomain

module K = Kat

type relation_t = Int of AbstractValue.t 
  | Bool of AbstractValue.t * AbstractValue.t
  | Unit of unit
type array_t = (token_t * token_t) * (relation_t * relation_t)

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
  type t = token_t
  let compare = compare
  end)

module VarMap = struct
  include TempVarMap
  let find key m = try TempVarMap.find key m 
    with Not_found -> raise (Key_not_found ("key is not Found in VarMap"))
end

module TempENodeMap = Map.Make(struct
  type t = token_t * token_t
  let compare = compare
end)

module TableMap = struct
  include TempENodeMap
  let find cs m = let _, key = cs in
    try TempENodeMap.find cs m 
    with Not_found -> raise (Key_not_found ((get_token_data key)^"is not Found in TableMap"))
  let compare = compare
end

module type SemanticsType =
  sig
    type node_t
    type node_s_t
    type env_t = (node_t * bool) VarMap.t
    type table_t
    type call_site
    type value_t = | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and list_t = (token_t * token_t) * (relation_t * value_t * K.expr' )
    and tuple_t = value_t list
    val init_T: (token_t * token_t) -> table_t
    val alpha_rename_T: (value_t -> token_t -> token_t -> value_t) -> table_t -> token_t -> token_t -> table_t
    val join_T: (value_t -> value_t -> value_t) -> (value_t -> token_t -> token_t -> value_t) -> table_t -> table_t -> table_t
    val meet_T: (value_t -> value_t -> value_t) -> (value_t -> token_t -> token_t -> value_t) -> table_t -> table_t -> table_t
    val leq_T: (value_t -> value_t -> bool) -> table_t -> table_t -> bool
    val eq_T: (value_t -> value_t -> bool) -> table_t -> table_t -> bool
    val forget_T: (token_t -> value_t -> value_t) -> token_t -> table_t -> table_t
    val arrow_T: (token_t -> value_t -> value_t) -> (token_t -> value_t -> value_t -> value_t) -> token_t -> table_t -> value_t -> table_t
    val wid_T: (value_t -> value_t -> value_t) -> (value_t -> token_t -> token_t -> value_t) -> table_t -> table_t -> table_t
    val equal_T: (value_t -> token_t -> value_t) -> (value_t -> token_t -> token_t -> value_t) -> table_t -> token_t -> table_t
    val replace_T: (value_t -> token_t -> token_t -> value_t) -> table_t -> token_t -> token_t -> table_t
    val stren_T: (value_t -> value_t -> value_t) -> table_t -> value_t -> table_t
    val proj_T: (value_t -> token_t list -> value_t) -> (value_t -> token_t list) -> table_t -> token_t list -> table_t
    val bot_shape_T: (value_t -> value_t) -> table_t -> table_t
    val get_label_snode: node_s_t -> token_t
    val construct_vnode: env_t -> var -> (token_t * token_t) -> node_t
    val construct_enode: env_t -> loc -> (token_t * token_t) -> node_t
    val construct_snode: var -> node_t -> node_s_t
    val construct_table: (token_t * token_t) -> value_t * value_t -> table_t
    val io_T: (token_t * token_t) -> value_t -> value_t * value_t
    val get_vnode: node_t -> env_t * token_t * (token_t * token_t)
    val dx_T: value_t -> (token_t * token_t)
    val get_table_T: value_t -> table_t
    val print_node: node_t -> Format.formatter -> (Format.formatter -> (token_t * (node_t * bool)) list -> unit) -> unit
    val print_table: table_t -> Format.formatter -> (Format.formatter -> value_t -> unit) -> unit
    val compare_node: (var -> var -> int) -> node_s_t -> node_s_t -> int
    val prop_table: ((token_t * token_t) -> value_t * value_t -> value_t * value_t -> (value_t * value_t) * (value_t * value_t)) -> (value_t -> token_t -> token_t -> value_t) -> table_t -> table_t -> table_t * table_t
    val step_func: ((token_t * token_t) -> value_t * value_t -> 'a -> 'a) -> value_t -> 'a -> 'a
    val get_full_table_T: table_t -> (token_t * token_t) * (value_t * value_t)
    val get_table_by_cs_T: (token_t * token_t) -> table_t -> (value_t * value_t)
    val update_table: (token_t * token_t) -> value_t * value_t -> table_t -> table_t
    val table_isempty: table_t -> bool
    val table_mapi: ((token_t * token_t) -> value_t * value_t -> value_t * value_t) -> table_t -> table_t
  end

module NonSensitive: SemanticsType =
  struct
    type node_t = EN of env_t * token_t (*N = E x loc*)
    and env_t = (node_t * bool) VarMap.t (*E = Var -> N*)
    type node_s_t = SN of bool * token_t
    type value_t =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and table_t = token_t * value_t * value_t
    and list_t = (token_t * token_t) * (relation_t * value_t * K.expr')
    and tuple_t = value_t list
    type call_site = None (* Not used *)
    let init_T (var_token, _) = var_token, Bot, Bot
    let alpha_rename_T (f: value_t -> token_t -> token_t -> value_t) (t:table_t) (pre_var_token:token_t) (var_token:token_t) :table_t =
      let (z, vi, vo) = t in
        (z, f vi pre_var_token var_token, f vo pre_var_token var_token)
    let join_T f g (t1:table_t) (t2:table_t) = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let meet_T f g (t1:table_t) (t2:table_t) = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let leq_T f (z1, v1i, v1o) (z2, v2i, v2o) = z1 = z2 && f v1i v2i && f v1o v2o
    let eq_T f (z1, v1i, v1o) (z2, v2i, v2o) = 
        z1 = z2 && f v1i v2i && f v1o v2o
    let forget_T f var_token (t:table_t) = let (z, vi, vo) = t in (z, f var_token vi, f var_token vo)
    let arrow_T f1 f2 var_token (t:table_t) v =
        let (z, vi, vo) = t in
        let v' = f1 z v in
        (z, f2 var_token vi v, f2 var_token vo v')
    let wid_T f g (t1:table_t) (t2:table_t) = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
        let v2o' = g v2o z2 z1 in 
        (z1, f v1i v2i, f v1o v2o') 
      in t
    let equal_T f g (t:table_t) var_token = let (z, vi, vo) = t in
        let vo' = if z = var_token then 
          g vo z (Var_Token "z1")
          else vo in
        (z, f vi var_token, f vo' var_token)
    let replace_T f (t:table_t) var_token x = let (z, vi, vo) = t in
      (z, f vi var_token x, f vo var_token x)
    let stren_T f (t:table_t) ae = let (z, vi, vo) = t in
      (z, f vi ae, f vo ae)
    let proj_T f g (t:table_t) var_tokens = let (z, vi, vo) = t in
      let var_tokens_o = 
        let var_tokens = z :: var_tokens in
        List.append var_tokens (g vi)
      in
      (z, f vi var_tokens, f vo var_tokens_o)
    let get_label_snode n = let SN (_, e1) = n in e1
    let construct_vnode env label _ = EN (env, Var_Token label)
    let construct_enode env label _ = EN (env, Var_Token label)
    let construct_snode var_token (EN (_, label)) = SN (true, label)
    let construct_table (z,_) (vi,vo) = z, vi, vo
    let io_T _ v = match v with
      | Table (_, vi, vo) -> vi, vo
      | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let get_vnode = function
      | EN (env, l) -> env, l, (l,l)
    let dx_T v = match v with
        | Table (z,_,_) -> z,z
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | Table t -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a], " f (VarMap.bindings env); print_token ppf l ; Format.fprintf ppf ">@]"
    let print_table t ppf f = let (z, vi, vo) = t in
      Format.fprintf ppf "@["; print_token ppf z; Format.fprintf ppf ": (%a ->@ %a)@]" f vi f vo
    let compare_node comp n1 n2 = 
      let SN (_, e1) = n1 in
      let SN (_, e2) = n2 in
      comp (get_token_data e1) (get_token_data e2)
    let prop_table f g (t1:table_t) (t2:table_t) = 
      let alpha_rename t1 t2 = let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then t1, t2
        else (*a renaming*)
        let v2o' = g v2o z2 z1 in
        (z1, v1i, v1o), (z1, v2i, v2o') in
      let t1', t2' = alpha_rename t1 t2 in
      let (z1, v1i, v1o) = t1' and (z2, v2i, v2o) = t2' in
      let (v1i', v1o'), (v2i', v2o') = f (z1, z2) (v1i, v1o) (v2i, v2o) in
      let t1' = (z1, v1i', v1o') and t2' = (z2, v2i', v2o') in
      t1', t2'
    let step_func f v m = 
      let z, tl, tr = match v with
      | Table (z, vi, vo) -> z, vi, vo
      | _ -> raise (Invalid_argument "Should be a table when using io_T") in
      f (z,z) (tl, tr) m
    let get_full_table_T t = let (z, vi, vo) = t in
      (z,z), (vi, vo)
    let get_table_by_cs_T cs t = let (z, vi, vo) = t in (vi, vo)
    let update_table cs vio t = construct_table cs vio
    let table_isempty t = false
    let table_mapi f t = t
    let bot_shape_T f t = 
      let (z, vi, vo) = t in
      (z, f vi, f vo)
  end

module OneSensitive: SemanticsType =
  struct
    type enode_t = env_t * token_t * call_site (*N = E x loc*)
    and vnode_t = env_t * token_t * call_site (*Nx = E x token_t x stack*)
    and node_t = EN of enode_t | VN of vnode_t
    and env_t = (node_t * bool) VarMap.t (*E = Var -> N*)
    and call_site = token_t * token_t   (*stack = token_t * loc*)
    and node_s_t = SEN of (call_site * token_t) | SVN of (call_site * token_t) (* call site * label *)
    type value_t =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t (* [<call_site>: table_t ...]*)
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and table_t = (value_t * value_t) TableMap.t
    and list_t = (token_t * token_t) * (relation_t * value_t * K.expr')
    and tuple_t = value_t list
    let init_T var = TableMap.empty
    let alpha_rename_T f (mt:table_t) (pre_var_token:token_t) (var_token:token_t) = TableMap.map (fun (vi, vo) ->
      f vi pre_var_token var_token, f vo pre_var_token var_token) mt
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
        Opt.get_or_else (v1i = Bot && v1o = Bot)) mt1
    let eq_T f mt1 mt2 =
        TableMap.equal (fun (v1i, v1o) (v2i, v2o) -> f v1i v2i && f v1o v2o) mt1 mt2
    let forget_T f var mt = TableMap.map (fun (vi, vo) -> 
      f var vi, f var vo) mt
    let arrow_T f1 f2 var mt v = TableMap.mapi (fun cs (vi, vo) -> 
      let _, z = cs in
      let v' = f1 z v in
      f2 var vi v, f2 var vo v') mt
    let wid_T f g mt1 mt2 =
      TableMap.union (fun cs (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, f v1o v2o)) mt1 mt2
    let equal_T f g mt var = TableMap.map (fun (vi, vo) -> f vi var, f vo var) mt
    let replace_T f mt var x = TableMap.map (fun (vi, vo) -> f vi var x, f vo var x) mt
    let stren_T f mt ae = TableMap.map (fun (vi, vo) -> f vi ae, f vo ae) mt
    let proj_T f g mt vars = TableMap.mapi (fun cs (vi, vo) -> 
      let _, var = cs in
      let vars_o = 
        let vars = var :: vars in
        List.append vars (g vi)
      in
      f vi vars, f vo vars_o) mt
    let get_label_snode n = match n with 
      | SEN (_, l) -> l
      | SVN (_, xl) -> xl
    let get_var_env_node = function
    | VN(env, l, var) -> env, l, var
    | EN(env, l, loc) -> raise (Invalid_argument ("Expected variable node at "^(get_token_data l)))
    let get_en_env_node = function
      | VN(env, l, var) -> raise (Invalid_argument ("Expected normal node at "^(get_token_data l)))
      | EN(env, l, loc) -> env, l, loc
    let get_var_s_node = function
      | SVN ((varx, var), xl) -> varx, var, xl
      | SEN(var, l) -> raise (Invalid_argument ("Expected variable node at "^(get_token_data l)))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^(get_token_data l)))
      | SEN(var, l) -> var, l
    let construct_vnode env label callsite = VN (env, Var_Token label, callsite)
    let construct_enode env label call_site = EN (env, Var_Token label, call_site)
    let construct_snode (x: var) (n:node_t): node_s_t = match n with
    | EN (env, l, (cx, cl)) -> if VarMap.is_empty env || x = "" then SEN ((Var_Token x, l), l) else
      let _, _, (_, va) = 
        let n, _ = VarMap.find (Var_Token x) env in
        get_var_env_node n in
      SEN ((va, l), l)
    | VN (env, xl, (cx, cl)) -> SVN ((cx, cl), xl)
    let construct_table cs (vi,vo) = TableMap.singleton cs (vi, vo)
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l, cs) -> raise (Invalid_argument ("Expected variable node at "^(get_token_data l)))
    let io_T cs v = match v with
        | Table t -> TableMap.find cs t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let dx_T v = match v with
        | Table t -> let cs, _ = try TableMap.min_binding t with
          Not_found -> (Var_Token "",Var_Token ""), (Bot,Bot) in cs
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | Table t -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = 
      let env, l, cs = match n with
        | EN (env, l, cs) -> env, l, cs
        | VN (env, l, cs) ->  env, l, cs
        in
      let _, var = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@], "
        f (VarMap.bindings env); print_token ppf var; print_token ppf l; Format.fprintf ppf "]"
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
        let l, var = cs in
        Format.fprintf ppf "@[<2>"; print_token ppf var; Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table t
      in print_table_map ppf t
    let compare_node comp n1 n2 = 
      let var1, var2, e1, e2 = match n1, n2 with
        | SEN ((_, var1), e1), SEN ((_, var2), e2) -> var1, var2, e1, e2
        | SEN ((_, var1), e1), SVN ((_, var2), e2) -> var1, var2, e1, e2
        | SVN ((_, var1), e1), SEN ((_, var2), e2) -> var1, var2, e1, e2
        | SVN ((_, var1), e1), SVN ((_, var2), e2) -> var1, var2, e1, e2
      in
      if comp (get_token_data e1) (get_token_data e2) = 0 then
        String.compare (get_token_data var1) (get_token_data var2) else comp (get_token_data e1) (get_token_data e2)
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
  end

module NSensitive: SemanticsType =
  struct
    type enode_t = env_t * token_t * call_site (*N = E x loc*)
    and vnode_t = env_t * token_t * call_site (*Nx = E x token_t x stack*)
    and node_t = EN of enode_t | VN of vnode_t
    and env_t = (node_t * bool) VarMap.t (*E = Var -> N*)
    and call_site = token_t * token_t   (*stack = token_t * loc*)
    and node_s_t = SEN of (call_site * token_t) | SVN of (call_site * token_t) (* call site * label *)
    type value_t =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t (* [<call_site>: table_t ...]*)
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and table_t = (value_t * value_t) TableMap.t (* TODO: sorted assoc list?*)
    and list_t = (token_t * token_t) * (relation_t * value_t * K.expr')
    and tuple_t = value_t list
    let init_T _ = TableMap.empty
    let alpha_rename_T f (mt:table_t) (pre_var_token:token_t) (var_token:token_t) = TableMap.map (fun (vi, vo) ->
      f vi pre_var_token var_token, f vo pre_var_token var_token) mt

    let get_traces_from_table table = List.map (fun ((var_token, trace), _) -> var_token, get_trace trace) table
    let trace_to_var_trace (var_token, trace) = None_Loc_Token (get_token_data var_token) :: trace
    let var_trace_to_trace trace = (Var_Token (List.hd trace |> get_loc_token_loc), Loc_Trace (List.tl trace))
    
    let rec add_traces_to_table bindings trace_trees table_add_function table = match bindings with
      | [] -> table
      | ((token, trace_token), (vi, vo)) :: tail ->
        let trace = trace_to_var_trace (token, get_trace (trace_token)) in
        let tree = List.hd trace |> find_head_in_tree_list trace_trees in
        try 
          let common_trace = find_trace_tree_longest_common_trace trace tree |> var_trace_to_trace |> snd in 
          TableMap.update (token, common_trace) (table_add_function (vi, vo)) table  
        with
          Not_found -> table
    
    let join_T f g mt1 mt2 =
      let table_add_function = (fun (vi, vo) v -> match v with | None -> Some (vi, vo) | Some (v1i, v1o) -> Some (f vi v1i, f vo v1o)) in
      let var_traces1 = TableMap.bindings mt1 |> get_traces_from_table |> List.map trace_to_var_trace in
      let var_traces2 = TableMap.bindings mt2 |> get_traces_from_table |> List.map trace_to_var_trace in
      let joined_var_traces = join_traces var_traces1 var_traces2 in
      let var_trace_trees = sort_trees joined_var_traces in
      add_traces_to_table (TableMap.bindings mt1) var_trace_trees table_add_function TableMap.empty
        |> add_traces_to_table (TableMap.bindings mt2) var_trace_trees table_add_function
    
    let meet_T f g mt1 mt2 = 
      let var_traces1 = TableMap.bindings mt1 |> get_traces_from_table |> List.map trace_to_var_trace in
      let var_traces2 = TableMap.bindings mt2 |> get_traces_from_table |> List.map trace_to_var_trace in
      let met_var_traces = meet_traces var_traces1 var_traces2 in
      let var_trace_trees = sort_trees met_var_traces in
      add_traces_to_table (TableMap.bindings mt1) var_trace_trees 
          (fun (vi, vo) v -> match v with | None -> Some (vi, vo) | Some (v1i, v1o) -> Some (f vi v1i, f vo v1o)) TableMap.empty 
        |> add_traces_to_table (TableMap.bindings mt2) var_trace_trees 
          (fun (vi, vo) v -> match v with | None -> None | Some (v1i, v1o) -> Some (f vi v1i, f vo v1o))

    let leq_T f mt1 mt2 = 
      let var_traces1 = TableMap.bindings mt1 |> get_traces_from_table |> List.map trace_to_var_trace in
      let var_traces2 = TableMap.bindings mt2 |> get_traces_from_table |> List.map trace_to_var_trace in
      let var_trees2 = sort_traces var_traces2 |> collect_traces in
      List.for_all (fun var_trace  -> 
        try
          let var, trace = var_trace_to_trace var_trace in
          let v1i, v1o = TableMap.find (var, trace) mt1 in
          let var_tree = List.hd var_trace |> find_head_in_tree_list var_trees2 in
          let super_var_traces = find_trace_tree_super_traces var_trace var_tree in
          if List.length super_var_traces = 0 then false (* TODO: should i join all values and then compare? *)
            else List.for_all (fun super_var_trace -> 
              let var, trace = var_trace_to_trace super_var_trace in
              let v2i, v2o = TableMap.find (var, trace) mt2 in
              f v1i v2i && f v1o v2o
            ) super_var_traces
        with Not_found -> false
      ) var_traces1

    let eq_T f mt1 mt2 =
        TableMap.equal (fun (v1i, v1o) (v2i, v2o) -> f v1i v2i && f v1o v2o) mt1 mt2

    let forget_T f var mt = TableMap.map (fun (vi, vo) -> 
      f var vi, f var vo) mt

    (* Todo: understand cs *)
    let arrow_T f1 f2 var mt v = TableMap.mapi (fun cs (vi, vo) -> 
      let _, z = cs in
      let v' = f1 z v in
      f2 var vi v, f2 var vo v') mt
    
    (* TODO: how to widen? *)
    let wid_T f g mt1 mt2 =
      TableMap.union (fun cs (v1i, v1o) (v2i, v2o) -> Some (f v1i v2i, f v1o v2o)) mt1 mt2

    let equal_T f g mt var = TableMap.map (fun (vi, vo) -> f vi var, f vo var) mt
    let replace_T f mt var x = TableMap.map (fun (vi, vo) -> f vi var x, f vo var x) mt
    let stren_T f mt ae = TableMap.map (fun (vi, vo) -> f vi ae, f vo ae) mt
    
    (* TODO: proj_T-construct_snode : understand cs*)
    let proj_T f g mt vars = TableMap.mapi (fun cs (vi, vo) -> 
      let _, var = cs in
      let vars_o = 
        let vars = var :: vars in
        List.append vars (g vi)
      in
      f vi vars, f vo vars_o) mt
    let get_label_snode n = match n with 
      | SEN (_, l) -> l
      | SVN (_, xl) -> xl
    let get_var_env_node = function
    | VN(env, l, var) -> env, l, var
    | EN(env, l, loc) -> raise (Invalid_argument ("Expected variable node at "^(get_token_data l)))
    let get_en_env_node = function
      | VN(env, l, var) -> raise (Invalid_argument ("Expected normal node at "^(get_token_data l)))
      | EN(env, l, loc) -> env, l, loc
    let get_var_s_node = function
      | SVN ((varx, var), xl) -> varx, var, xl
      | SEN(var, l) -> raise (Invalid_argument ("Expected variable node at "^(get_token_data l)))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^(get_token_data l)))
      | SEN(var, l) -> var, l
    let construct_vnode env label callsite = VN (env, Var_Token label, callsite)
    let construct_enode env label call_site = EN (env, Var_Token label, call_site)
    let construct_snode (x: var) (n:node_t): node_s_t = match n with
    | EN (env, l, (cx, cl)) -> if VarMap.is_empty env || x = "" then SEN ((Var_Token x, Var_Token x), l) else
      let _, _, (_, va) = 
        let n, _ = VarMap.find (Var_Token x) env in
        get_var_env_node n in
      SEN ((va, l), l)
    | VN (env, xl, (cx, cl)) -> SVN ((cx, cl), xl)
    let construct_table cs (vi,vo) = TableMap.singleton cs (vi, vo)
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l, cs) -> raise (Invalid_argument ("Expected variable node at "^(get_token_data l)))
    let io_T cs v = match v with
        | Table t -> TableMap.find cs t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    
    (* Todo: understand cs *)
    let dx_T v = match v with
        | Table t -> let cs, _ = try TableMap.min_binding t with
          Not_found -> (Var_Token "",Var_Token ""), (Bot,Bot) in cs
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | Table t -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = 
      let env, l, cs = match n with
        | EN (env, l, cs) -> env, l, cs
        | VN (env, l, cs) ->  env, l, cs
        in
      let var, trace = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@], "
        f (VarMap.bindings env); print_token ppf var; print_token ppf trace ; print_token ppf l; Format.fprintf ppf "]"
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
        let l, var = cs in
        Format.fprintf ppf "@[<2>"; print_token ppf var; Format.fprintf ppf ":@ @[<2>%a@]@]" pr_table t
      in print_table_map ppf t
    let compare_node comp n1 n2 = 
      let var1, var2, e1, e2 = match n1, n2 with
        | SEN ((_, var1), e1), SEN ((_, var2), e2) -> var1, var2, e1, e2
        | SEN ((_, var1), e1), SVN ((_, var2), e2) -> var1, var2, e1, e2
        | SVN ((_, var1), e1), SEN ((_, var2), e2) -> var1, var2, e1, e2
        | SVN ((_, var1), e1), SVN ((_, var2), e2) -> var1, var2, e1, e2
      in
      if comp (get_token_data e1) (get_token_data e2) = 0 then
        String.compare (get_token_data var1) (get_token_data var2) else comp (get_token_data e1) (get_token_data e2)
    
    (* Todo: after discussion *)
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
end

let parse_sensitive = function
  | false -> (module NonSensitive: SemanticsType)
  | _ -> (module OneSensitive: SemanticsType)

module SenSemantics = (val (!sensitive |> parse_sensitive))

module TempNodeMap = MakeHash(struct
  type t = SenSemantics.node_s_t
  end)

module NodeMap = struct
  include TempNodeMap
  let find key (m: 'a t): 'a = let e1 = SenSemantics.get_label_snode key in
    try TempNodeMap.find key m
    with Not_found -> raise (Key_not_found ((get_token_data e1)^" is not Found in NodeMap"))
end

type exec_map_t = SenSemantics.value_t NodeMap.t
