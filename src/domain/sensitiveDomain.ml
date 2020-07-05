open AbstractDomain
open Syntax
open Util
open Config

let int_of_bool b = if b then 1 else 0

type stack_t = var * loc
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
  type t = stack_t
  let compare = compare
end)

module TableMap = struct
  include TempENodeMap
  let find cs m = let _, key = cs in
    try TempENodeMap.find cs m 
    with Not_found -> raise (Key_not_found (key^" is not Found in TableMap"))
  let compare = compare
end

module type SensitiveSemanticsType =
  sig
    type node_t
    type node_s_t
    type env_t = node_t VarMap.t
    type table_t
    type call_site
    type value_t = | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and list_t = (var * var) * (relation_t * value_t)
    and tuple_t = value_t list
    val init_T: (var * loc) -> table_t
    val alpha_rename_T: (value_t -> string -> string -> value_t) -> table_t -> string -> string -> table_t
    val join_T: (value_t -> value_t -> value_t) -> (value_t -> string -> string -> value_t) -> table_t -> table_t -> table_t
    val meet_T: (value_t -> value_t -> value_t) -> (value_t -> string -> string -> value_t) -> table_t -> table_t -> table_t
    val leq_T: (value_t -> value_t -> bool) -> table_t -> table_t -> bool
    val eq_T: (value_t -> value_t -> bool) -> table_t -> table_t -> bool
    val forget_T: (var -> value_t -> value_t) -> var -> table_t -> table_t
    val arrow_T: (var -> value_t -> value_t) -> (var -> value_t -> value_t -> value_t) -> var -> table_t -> value_t -> table_t
    val wid_T: (value_t -> value_t -> value_t) -> (value_t -> string -> string -> value_t) -> table_t -> table_t -> table_t
    val equal_T: (value_t -> var -> value_t) -> (value_t -> string -> string -> value_t) -> table_t -> var -> table_t
    val replace_T: (value_t -> var -> var -> value_t) -> table_t -> var -> var -> table_t
    val stren_T: (value_t -> value_t -> value_t) -> table_t -> value_t -> table_t
    val proj_T: (value_t -> string list -> value_t) -> (value_t -> string list) -> table_t -> string list -> table_t
    val bot_shape_T: (value_t -> value_t) -> table_t -> table_t
    val get_label_snode: node_s_t -> string
    val construct_vnode: env_t -> loc -> (var * loc) -> node_t
    val construct_enode: env_t -> loc -> node_t
    val construct_snode: var -> node_t -> node_s_t
    val construct_table: (var * loc) -> value_t * value_t -> table_t
    val io_T: (var * loc) -> value_t -> value_t * value_t
    val get_vnode: node_t -> env_t * loc * (var * loc)
    val dx_T: value_t -> (var * loc)
    val get_table_T: value_t -> table_t
    val print_node: node_t -> Format.formatter -> (Format.formatter -> (string * node_t) list -> unit) -> unit
    val print_table: table_t -> Format.formatter -> (Format.formatter -> value_t -> unit) -> unit
    val compare_node: (string -> string -> int) -> node_s_t -> node_s_t -> int
    val prop_table: ((var * loc) -> value_t * value_t -> value_t * value_t -> (value_t * value_t) * (value_t * value_t)) -> (value_t -> string -> string -> value_t) -> table_t -> table_t -> table_t * table_t
    val step_func: ((var * loc) -> value_t * value_t -> 'a -> 'a) -> value_t -> 'a -> 'a
    val get_full_table_T: table_t -> (var * loc) * (value_t * value_t)
    val get_table_by_cs_T: (var * loc) -> table_t -> (value_t * value_t)
    val update_table: (var * loc) -> value_t * value_t -> table_t -> table_t
    val table_isempty: table_t -> bool
    val table_mapi: ((var * loc) -> value_t * value_t -> value_t * value_t) -> table_t -> table_t
  end

module NonSensitive: SensitiveSemanticsType =
  struct
    type node_t = EN of env_t * loc (*N = E x loc*)
    and env_t = node_t VarMap.t (*E = Var -> N*)
    type node_s_t = SN of bool * loc
    type value_t =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and table_t = var * value_t * value_t
    and list_t = (var * var) * (relation_t * value_t)
    and tuple_t = value_t list
    type call_site = None (* Not used *)
    let init_T (var, _) = var, Bot, Bot
    let alpha_rename_T (f: value_t -> string -> string -> value_t) (t:table_t) (prevar:string) (var:string) :table_t = 
      let (z, vi, vo) = t in
        (z, f vi prevar var, f vo prevar var)
    let join_T f g (t1:table_t) (t2:table_t) = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let meet_T f g t1 t2 = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
          let v2o' = g v2o z2 z1 in 
          (z1, f v1i v2i, f v1o v2o')
        in t
    let leq_T f (z1, v1i, v1o) (z2, v2i, v2o) = z1 = z2 && f v1i v2i && f v1o v2o
    let eq_T f (z1, v1i, v1o) (z2, v2i, v2o) = 
        z1 = z2 && f v1i v2i && f v1o v2o
    let forget_T f var t = let (z, vi, vo) = t in (z, f var vi, f var vo)
    let arrow_T f1 f2 var t v =
        let (z, vi, vo) = t in
        let v' = f1 z v in
        (z, f2 var vi v, f2 var vo v')
    let wid_T f g t1 t2 = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, f v1i v2i, f v1o v2o) else (*a renaming*)
        let v2o' = g v2o z2 z1 in 
        (z1, f v1i v2i, f v1o v2o') 
      in t
    let equal_T f g t var = let (z, vi, vo) = t in
        let vo' = if z = var then 
          g vo z "z1"
          else vo in
        (z, f vi var, f vo' var)
    let replace_T f t var x = let (z, vi, vo) = t in
      (z, f vi var x, f vo var x)
    let stren_T f t ae = let (z, vi, vo) = t in
      (z, f vi ae, f vo ae)
    let proj_T f g t vars = let (z, vi, vo) = t in
      let vars_o = 
        let vars = z :: vars in
        List.append vars (g vi)
      in
      (z, f vi vars, f vo vars_o)
    let get_label_snode n = let SN (_, e1) = n in e1
    let construct_vnode env label _ = EN (env, label)
    let construct_enode env label = EN (env, label)
    let construct_snode var (EN (_, label)) = SN (true, label)
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
      | EN (env, l) -> Format.fprintf ppf "@[<1><[%a], %s>@]" f (VarMap.bindings env) l
    let print_table t ppf f = let (z, vi, vo) = t in
      Format.fprintf ppf "@[<2>%s: (%a ->@ %a)@]" z f vi f vo
    let compare_node comp n1 n2 = 
      let SN (_, e1) = n1 in
      let SN (_, e2) = n2 in
      comp e1 e2
    let prop_table f g t1 t2 = 
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

module OneSensitive: SensitiveSemanticsType =
  struct
    type enode_t = env_t * loc (*N = E x loc*)
    and vnode_t = env_t * var * call_site (*Nx = E x var x stack*)
    and node_t = EN of enode_t | VN of vnode_t
    and env_t = node_t VarMap.t (*E = Var -> N*)
    and call_site = var * loc   (*stack = var * loc*)
    and node_s_t = SEN of call_site | SVN of (call_site * var) (* call site * label *)
    type value_t =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t (* [<call_site>: table_t ...]*)
      | Ary of array_t
      | Lst of list_t
      | Tuple of tuple_t
    and table_t = (value_t * value_t) TableMap.t
    and list_t = (var * var) * (relation_t * value_t)
    and tuple_t = value_t list
    let init_T var = TableMap.empty
    let alpha_rename_T f (mt:table_t) (prevar:string) (var:string) = TableMap.map (fun (vi, vo) -> 
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
    | EN(env, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let get_en_env_node = function
      | VN(env, l, var) -> raise (Invalid_argument ("Expected normal node at "^l))
      | EN(env, l) -> env, l
    let get_var_s_node = function
      | SVN ((varx, var), xl) -> varx, var, xl
      | SEN(var, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let get_en_s_node = function
      | SVN(_, l) -> raise (Invalid_argument ("Expected normal node at "^l))
      | SEN(var, l) -> var, l
    let construct_vnode env label callsite = VN (env, label, callsite)
    let construct_enode env label = EN (env, label)
    let construct_snode (x: var) (n:node_t): node_s_t = match n with
    | EN (env, l) -> if VarMap.is_empty env || x = "" then SEN ("", l) else
      let _, _, (_, va) = VarMap.find x env |> get_var_env_node in
      SEN (va, l)
    | VN (env, xl, (cx, cl)) -> SVN ((cx, cl), xl)
    let construct_table cs (vi,vo) = TableMap.singleton cs (vi, vo)
    let get_vnode = function
      | VN(env, l, cs) -> env, l, cs
      | EN(_, l) -> raise (Invalid_argument ("Expected variable node at "^l))
    let io_T cs v = match v with
        | Table t -> TableMap.find cs t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    let dx_T v = match v with
        | Table t -> let cs, _ = try TableMap.min_binding t with
          Not_found -> ("",""), (Bot,Bot) in cs
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
    let get_table_T = function
      | Table t -> t
      | _ -> raise (Invalid_argument "Should be a table when using get_table_T")
    let print_node n ppf f = match n with
      | EN (env, l) -> Format.fprintf ppf "@[[<1><[%a], %s>@]" f (VarMap.bindings env) l
      | VN (env, l, cs) ->  let _, var = cs in Format.fprintf ppf "@[<1><@[<1>[%a]@],@ %s,@ %s>@]"
        f (VarMap.bindings env) var l
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
        Format.fprintf ppf "@[<2>%s:@ @[<2>%a@]@]" var pr_table t
      in print_table_map ppf t
    let compare_node comp n1 n2 = match n1, n2 with
      | SEN (_, e1), SEN (_, e2) -> comp e1 e2
      | SEN (_, e1), SVN (_, e2) -> comp e1 e2
      | SVN (_, e1), SEN (_, e2) -> comp e1 e2
      | SVN ((_, var1), e1), SVN ((_, var2), e2) -> 
        if comp e1 e2 = 0 then
          String.compare var1 var2 else comp e1 e2
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
  | 0 -> (module NonSensitive: SensitiveSemanticsType)
  | 1 -> (module OneSensitive: SensitiveSemanticsType)
  | _ -> raise (Invalid_argument "Incorrect sensitive specification")

module SenSemantics = (val (!sensitive |> int_of_bool |> parse_sensitive))

module TempNodeMap = MakeHash(struct
  type t = SenSemantics.node_s_t
  end)

module NodeMap = struct
  include TempNodeMap
  let find key (m: 'a t): 'a = let e1 = SenSemantics.get_label_snode key in
    try TempNodeMap.find key m
    with Not_found -> raise (Key_not_found (e1^" is not Found in NodeMap"))
end

type exec_map_t = SenSemantics.value_t NodeMap.t


(* module type SensitiveSemanticsType =
  sig
    type node_t
    type node_s_t
    type table_t
    type value_t = | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Unit of unit
    val init_T: var -> table_t
    val alpha_rename_T: (value_t -> string -> string -> value_t) -> table_t -> string -> string -> table_t
    val join_T: (value_t -> value_t -> value_t) -> table_t -> table_t -> table_t
    val meet_T: (value_t -> value_t -> value_t) -> table_t -> table_t -> table_t
    val leq_T: (value_t -> value_t -> bool) -> table_t -> table_t -> bool
    val eq_T: (value_t -> value_t -> bool) -> table_t -> table_t -> bool
    val forget_T: (var -> value_t -> value_t) -> var -> table_t -> table_t
    val arrow_T: (var -> value_t -> value_t) -> (var -> value_t -> value_t -> value_t) -> var -> table_t -> value_t -> table_t
    val wid_T: (value_t -> value_t -> value_t) -> table_t -> table_t -> table_t
    val equal_T: (value_t -> var -> value_t) -> table_t -> var -> table_t
    val replace_T: (value_t -> var -> var -> value_t) -> table_t -> var -> var -> table_t
    val stren_T: (value_t -> value_t -> value_t) -> table_t -> value_t -> table_t
    val proj_T: (value_t -> string array -> value_t) -> table_t -> string array -> table_t
    val get_label_T: node_s_t -> string
  end *)

(* module MakeTableSemantics (Man: SenManagerType): SensitiveSemanticsType = 
  struct
  type node_t = Man.node
  type node_s_t = Man.node_s
  type table_t = Man.table
  type value_t = | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Unit of unit
  let init_T var = Man.init var
  let alpha_rename_T f t prevar var = Man.alpha_rename f t prevar var 
  let join_T f t1 t2 =  Man.join f t1 t2
  let meet_T f t1 t2 = Man.meet f t1 t2
  let leq_T f t1 t2 = Man.leq f t1 t2
  let eq_T f t1 t2 = Man.eq f t1 t2
  let forget_T f var t = Man.forget f var t
  let arrow_T f1 f2 var t v = Man.arrow f1 f2 var t v
  let wid_T f t1 t2 = Man.wid f t1 t2
  let equal_T f t var = Man.equal f t var
  let replace_T f t var x = Man.replace f t var x
  let stren_T f t ae = Man.stren f t ae
  let proj_T f t vars = Man.proj f t vars
  let get_label_T n = Man.get_label n
  end *)