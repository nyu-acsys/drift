open AbstractDomain
open Syntax
open Util

(*
**********************************
** Abstract Data Flow Semantics **
**********************************
*)

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

type node_t = EN of env_t * loc (*N = E x loc*)
and env_t = node_t VarMap.t (*E = Var -> N*)

type node_s_t = SN of bool * loc (*N = E x loc*)

module TempNodeMap = MakeHash(struct
  type t = node_s_t
  end)

module NodeMap = struct
  include TempNodeMap
  let find key (m: 'a t): 'a = let SN (_, e1) = key in
    try TempNodeMap.find key m
    with Not_found -> raise (Key_not_found (e1^" is not Found in NodeMap"))
end

let name_of_node lb = ("z" ^ lb)

let l_index = ref 0

let incr_l () =
  l_index := !l_index + 1

let comp s1 s2 =
  let l1 = try int_of_string s1 
    with _ -> -1 in
  let l2 = try int_of_string s2
    with _ -> -1 in
  if l1 = -1 then
    if l2 = -1 then String.compare s1 s2
    else -1
  else if l2 = -1 then 1
  else l1 - l2

module SemanticsDomain =
  struct
    (*
    **********************************
    ** Abstract domain for Relation **
    **********************************
    *)
    type relation_t = Int of AbstractValue.t
      | Bool of AbstractValue.t * AbstractValue.t
    type value_t =
      | Bot
      | Top
      | Relation of relation_t
      | Table of table_t
      | Ary of array_t
      | Unit of unit
    and array_t = (var array) * relation_t (* disable element for now*)
    and table_t = var * value_t * value_t
    and exec_map_t = value_t NodeMap.t
    let rec alpha_rename_R a prevar var = match a with
        | Int v -> Int (AbstractValue.alpha_rename v prevar var)
        | Bool (vt, vf) -> Bool ((AbstractValue.alpha_rename vt prevar var), (AbstractValue.alpha_rename vf prevar var))
    and top_R = function
    | Plus | Mult | Div | Mod | Minus -> (Int AbstractValue.top)
    | Ge | Eq | Ne | Lt | Gt | Le | And | Or -> (Bool (AbstractValue.top, AbstractValue.top))
    and bot_R = function
    | Plus | Mult | Div | Mod | Minus -> (Int AbstractValue.bot)
    | Ge | Eq | Ne | Lt | Gt | Le | And | Or -> (Bool (AbstractValue.bot, AbstractValue.bot))
    and is_bool_R a = match a with
        | Int _ -> false
        | Bool _ -> true
    and init_R_c (c:value) = match c with
        | Boolean true -> Bool (AbstractValue.init_c 1, AbstractValue.bot)
        | Boolean false -> Bool (AbstractValue.bot, AbstractValue.init_c 0)
        | Integer i -> Int (AbstractValue.init_c i)
    and join_R a1 a2 =
        match a1, a2 with
          | (Int v1), (Int v2) -> Int (AbstractValue.join v1 v2)
          | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> Bool (AbstractValue.join v1t v2t, AbstractValue.join v1f v2f)
          | _, _ -> raise (Invalid_argument "Join: Base Type not equal")
    and meet_R a1 a2 =
        match a1, a2 with
          | (Int v1), (Int v2) -> Int  (AbstractValue.meet v1 v2)
          | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> Bool (AbstractValue.meet v1t v2t, AbstractValue.meet v1f v2f)
          | _, _ -> raise (Invalid_argument "Meet: Base Type not equal")
    and leq_R a1 a2 =
        match a1, a2 with
          | (Int v1), (Int v2) -> AbstractValue.leq v1 v2
          | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> AbstractValue.leq v1t v2t && AbstractValue.leq v1f v2f
          | _, _ -> false
    and eq_R a1 a2 =
      match a1, a2 with
        | (Int v1), (Int v2) -> AbstractValue.eq v1 v2
        | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> AbstractValue.eq v1t v2t && AbstractValue.eq v1f v2f
        | _, _ -> false
    and arrow_R var a1 a2 = 
        let a2' = alpha_rename_R a2 "cur_v" var in
        match a1, a2' with
        | Int _, Int _ -> meet_R a1 a2'
        | Bool (vt1, vf1), Bool (vt2, vf2) -> (* {v:bool | at: [at^at' V at^af'], af: [af^at' V af^af']} *)
          let vt1' = AbstractValue.join (AbstractValue.meet vt1 vt2) (AbstractValue.meet vt1 vf2) in
          let vf1' = AbstractValue.join (AbstractValue.meet vf1 vt2) (AbstractValue.meet vf1 vf2) in
          Bool (vt1', vf1')
        | Int v, Bool (vt, vf) -> (* {v:int| a^at V a^af} *)
          Int (AbstractValue.join (AbstractValue.meet v vt) (AbstractValue.meet v vf))
        | Bool _ , Int v -> meet_R a1 (Bool (v, v))
    and forget_R var a = match a with
        | Int v -> Int (AbstractValue.forget_var var v)
        | Bool (vt, vf) -> Bool (AbstractValue.forget_var var vt, AbstractValue.forget_var var vf)
    and equal_R a var = let eq_a = match a with
      | Int v -> Int (AbstractValue.equal_var v "cur_v" var)
      | Bool (vt, vf) -> Bool ((AbstractValue.equal_var vt "cur_v" var), (AbstractValue.equal_var vf "cur_v" var))
      in
      meet_R a eq_a
    and wid_R a1 a2 = match a1, a2 with
      | (Int v1), (Int v2) -> Int (AbstractValue.widening v1 v2)
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> Bool (AbstractValue.widening v1t v2t, AbstractValue.widening v1f v2f)
      | _, _ -> raise (Invalid_argument "Widening: Base Type not equal")
    and op_R l r op cons a = (*cons for flag of linear constraints*)
      match op with
      | Plus | Mult | Div | Mod | Minus -> (match a with
        | Int v -> Int (AbstractValue.operator l r op (-1) v)
        | Bool (vt, vf) -> raise (Invalid_argument "Conditional value given, expect arithmetic one"))
      | Ge | Eq | Ne | Lt | Gt | Le -> (if cons then
        (match a with
        | Int v -> Int (AbstractValue.operator l r op (-1) v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator l r op 1 vt, AbstractValue.operator l r (rev_op op) 0 vf)
        )
        else
        (match a with
        | Int v -> Bool (AbstractValue.operator l r op 1 v, AbstractValue.operator l r (rev_op op) 0 v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator l r op 1 vt, AbstractValue.operator l r (rev_op op) 0 vf))
      )
      | And | Or -> raise (Invalid_argument "Invalid method called, should use bool_op_R for conditional operators")
    and bool_op_R op a1 a2 = match a1, a2 with
    | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> if string_of_op op = "&&" then
      Bool (AbstractValue.meet v1t v2t, AbstractValue.join v1f v2f)
    else
      Bool (AbstractValue.join v1t v2t, AbstractValue.meet v1f v2f)
    | _, _ -> raise (Invalid_argument "&& or || operation: Base Type should be bool")
    and replace_R a var x = alpha_rename_R a var x
    and extrac_bool_R v b = match v,b with
    | Bool (vt, _), true -> Int vt |> forget_R "cur_v"
    | Bool (_, vf), false -> Int vf |> forget_R "cur_v"
    | _,_ -> raise (Invalid_argument "Extract abstract value for condition, expect bool one")
    and stren_R a ae = 
      match a, ae with
    | Int v, Int vae -> Int (AbstractValue.meet v vae)
    | Bool (vt, vf), Int vae -> Bool (AbstractValue.meet vt vae, AbstractValue.meet vf vae)
    | _, _ -> raise (Invalid_argument "ae should be {v:Int}")
  and der_R exp a = match a with
    | Bool (vt, vf) -> Bool (AbstractValue.derived exp vt, AbstractValue.derived exp vf)
    | Int v -> Int (AbstractValue.derived exp v)
  and proj_R a vars = match a with
    | Int v -> Int (AbstractValue.project_other_vars v vars)
    | Bool (vt, vf) -> Bool (AbstractValue.project_other_vars vt vars, AbstractValue.project_other_vars vf vars)
  and is_bot_R a = match a with
    | Int v -> AbstractValue.is_bot v
    | Bool (vt, vf) -> AbstractValue.is_bot vt && AbstractValue.is_bot vf
  and is_bool_false_R a = match a with
    | Int _ -> raise (Invalid_argument "Expect a bool value, but got int")
    | Bool (vt, vf) -> AbstractValue.is_bot vf
  and opt_eq_R a1 a2 = is_bot_R a1 = false && is_bot_R a2 = false && eq_R a1 a2
    (*
    *******************************
    ** Abstract domain for Table **
    *******************************
    *)
    and init_T var = (var, Bot, Bot)
    and dx_Ta t = let (z, vi, vo) = t in z
    and io_Ta t = let (z, vi, vo) = t in vi, vo
    and alpha_rename_T t prevar var = let (z, vi, vo) = t in
        (z, alpha_rename_V vi prevar var, alpha_rename_V vo prevar var)
    and alpha_rename t1 t2 = let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then t1, t2
        else (*a renaming*)
        let v2o' = alpha_rename_V v2o z2 z1 in
        (z1, v1i, v1o), (z1, v2i, v2o')
    and join_T t1 t2 = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, join_V v1i v2i, join_V v1o v2o) else (*a renaming*)
            let v2o' = alpha_rename_V v2o z2 z1 in 
            (z1, join_V v1i v2i, join_V v1o v2o')
        in t
    and meet_T t1 t2 = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, meet_V v1i v2i, meet_V v1o v2o) else (*a renaming*)
            let v2o' = alpha_rename_V v2o z2 z1 in 
            (z1, meet_V v1i v2i, meet_V v1o v2o')
        in t
    and leq_T (z1, v1i, v1o) (z2, v2i, v2o) = 
        z1 = z2 && leq_V v1i v2i && leq_V v1o v2o
    and eq_T (z1, v1i, v1o) (z2, v2i, v2o) = 
        z1 = z2 && eq_V v1i v2i && eq_V v1o v2o
    and forget_T var t = let (z, vi, vo) = t in (z, forget_V var vi, forget_V var vo)
    and arrow_T var t v =
        let (z, vi, vo) = t in
        let v' = forget_V z v in
        (z, arrow_V var vi v, arrow_V var vo v')
    and wid_T t1 t2 = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, wid_V v1i v2i, wid_V v1o v2o) else (*a renaming*)
        let v2o' = alpha_rename_V v2o z2 z1 in 
        (z1, wid_V v1i v2i, wid_V v1o v2o') in t
    and equal_T t var = let (z, vi, vo) = t in
        let vo' = if z = var then 
          alpha_rename_V vo z "z1"
          else vo in
        (z, equal_V vi var, equal_V vo' var)
    and replace_T t var x = let (z, vi, vo) = t in
      (z, replace_V vi var x, replace_V vo var x)
    and stren_T t ae = let (z, vi, vo) = t in
      (z, stren_V vi ae, stren_V vo ae)
    and proj_T t vars = let (z, vi, vo) = t in
      let vars_o = Array.append vars [|z|] in
      (z, proj_V vi vars, proj_V vo vars_o)
    (*
      ***************************************
      ** Abstract domain for Execution Map **
      ***************************************
      *)
      and meet_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
        NodeMap.merge (fun n v1 v2 -> Some (meet_V v1 v2)) m1 m2
      and join_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
        NodeMap.union (fun n v1 v2 -> join_V v1 v2) m1 m2
      and wid_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
        NodeMap.union (fun n v1 v2 -> wid_V v1 v2) m1 m2
      and leq_M (m1: exec_map_t) (m2: exec_map_t) : bool =
        NodeMap.for_all (fun n v1 (*untie to node -> value*) -> 
        NodeMap.find_opt n m2 |> Opt.map (fun v2 -> leq_V v1 v2) |>
        Opt.get_or_else (v1 = Bot)) m1
      and eq_PM (m1:exec_map_t) (m2:exec_map_t) =
        NodeMap.for_all (fun n v1 (*untie to node -> value*) ->
        NodeMap.find_opt n m2 |> Opt.map (
          fun v2 -> let SN (_, l) = n in
          if eq_V v1 v2 then true else 
            raise (Pre_Def_Change ("Predefined node changed at " ^ l))
          )
        |> Opt.get_or_else true) m1
      and top_M m = NodeMap.map (fun a -> Top) m
      and array_M env m = 
        let n_make = EN (env, "Array.make") in
        let s_make = SN (true,  "Array.make") in
        let t_make = (* make *)
          (* make |-> zm: {v:int | v >= 0} -> ex: {v:int | top} -> {v: Int Array (l) | [| l=zm; zm>=0; |]} *)
          let var_l = "zm" in
          let rl = top_R Plus |> op_R "cur_v" "0" Ge true in
          let var_e = "ex" in
          let rm = top_R Plus in (*TODO: Make poly*)
          let llen = arrow_R var_l rm rl |> op_R "l" var_l Eq true in
          (* let ilen = op_R "i" "0" Ge true llen |> op_R "i" "l" Lt true in
          let rlen = op_R "x" var_e Eq true ilen in  *)
          Table (var_l, Relation rl, Table (var_e, Relation rm, Ary ([|"l";|],llen) ))
        in
        let n_len = EN (env, "Array.length") in
        let s_len = SN (true,  "Array.length") in
        let t_len = (* len *)
          (* len |-> zl: { v: Int Array (l) | [| l>=0; |]  } -> { v: Int | [| v=l |] } *)
          let var_l = "zl" in
          let var_len = "l" in
          (* let var_i = "i" in *)
          let llen = top_R Plus |> op_R var_len "0" Ge true in
          (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
          let rlen = equal_R (top_R Plus) "l" in
          Table (var_l, Ary ([|"l"|], llen), Relation rlen)
        in
        let n_get = EN (env, "Array.get") in
        let s_get = SN (true, "Array.get") in
        let t_get = (* get *)
          (* get |-> zg: { v: Int Array (l) | [| l>=0; |] } -> zi: {v: int | 0 <= v < l} -> {v: int | top }  *)
          let var_l = "zg" in
          let var_len = "l" in
          (* let var_i = "i" in *)
          let llen = replace_R (top_R Plus |> op_R "cur_v" "0" Ge true) "cur_v" var_len in
          (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
          let rm = top_R Plus |> op_R "cur_v" "0" Ge true |> op_R "cur_v" var_len Lt true in
          let rr = top_R Plus (*|> op_R "cur_v" "x" Eq true*) in 
          let var_zi = "zi" in
          Table (var_l, Ary ([|"l"|], llen), Table (var_zi, Relation rm, Relation rr))
        in
        let n_set = EN (env, "Array.set") in
        let s_set = SN (true, "Array.set") in
        let t_set = (* set *)
          (* set |-> zs: { v: Int Array (l) | [| l>=0; |] } -> zi: {v: int | 0 <= v < l} -> ex: {v: int | top } -> unit *)
          let var_l = "zs" in
          let var_len = "l" in
          (* let var_i = "i" in *)
          let llen = replace_R (top_R Plus |> op_R "cur_v" "0" Ge true) "cur_v" var_len in
          (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
          let rm = top_R Plus |> op_R "cur_v" "0" Ge true |> op_R "cur_v" var_len Lt true in
          let var_e = "ex" in
          let var_zi = "zi" in
          let rri = top_R Plus in
          let rrr = () in
          Table (var_l, Ary ([|"l"|], llen), Table (var_zi, Relation rm, Table (var_e, Relation rri, Unit rrr)))
        in
        let m' = 
          m |> NodeMap.add s_make t_make |> NodeMap.add s_len t_len |> NodeMap.add s_get t_get
          |> NodeMap.add s_set t_set
        in
        let env' = 
          env |> VarMap.add "Array.make" n_make |> VarMap.add "Array.length" n_len |> VarMap.add "Array.get" n_get
          |> VarMap.add "Array.set" n_set
        in
        env', m'
      and pref_M env m = 
        let m', env' =
          if VarDefMap.is_empty !pre_vars then
            m, env
          else 
            VarDefMap.fold (fun var (domain: pre_exp) (m, env) -> 
              let n_var = EN (env, var) in
              let s_var = SN (true, var) in
              let t_var = match domain with
                | {name = n; dtype = Int; left = l; op = bop; right = r} -> 
                  let rm = if l = "true" then 
                     (top_R Plus)
                  else top_R Plus |> op_R l r bop true
                  in Relation rm
                | {name = n; dtype = Bool; left = l; op = bop; right = r} -> 
                  let rm = if l = "true" then init_R_c (Boolean true)
                    else if l = "false" then
                      init_R_c (Boolean false)
                    else
                      top_R Plus |> op_R l r bop true
                  in
                  Relation (rm)
                | {name = n; dtype = Unit; left = l; op = _; right = r} ->
                  if l = "unit" then Unit () 
                  else raise (Invalid_argument"Expected unit predicate as {v: Unit | unit }")
              in
              let env' = env |> VarMap.add var n_var in
              let m' = m |> NodeMap.add s_var t_var in
              m', env'
            ) !pre_vars (m, env)
        in env', m'
      (*
      ********************************
      ** Abstract domain for Values **
      ********************************
      *)
      and alpha_rename_V v prevar var = match v with
        | Relation r -> Relation (alpha_rename_R r prevar var)
        | Table t -> Table (alpha_rename_T t prevar var)
        | Ary ary -> Ary (alpha_rename_Ary ary prevar var)
        | _ -> v
      and init_V_c (c:value) = Relation (init_R_c c)
      and c_V v1 v2 label = match v2 with
        | Relation r -> (match v1 with
          | Relation r' -> let tempr = alpha_rename_R r "cur_v" label in
          (try Relation (meet_R r' tempr)
          with Invalid_argument s -> Relation r')
          | _ -> v1)
        | _ -> v1
      and join_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
        | Bot, v | v, Bot -> v
        | Relation r1, Relation r2 -> Relation (join_R r1 r2)
        | Table t1, Table t2 -> Table (join_T t1 t2)
        | Ary ary1, Ary ary2 -> Ary (join_Ary ary1 ary2)
        | Unit u1, Unit u2 -> Unit u1
        | _, _ -> Top
      and meet_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
        | Top, v | v, Top -> v
        | Relation r1, Relation r2 -> Relation (meet_R r1 r2)
        | Table t1, Table t2 -> Table (meet_T t1 t2)
        | Ary ary1, Ary ary2 -> Ary (meet_Ary ary1 ary2)
        | Unit u1, Unit u2 -> Unit u1
        | _, _ -> Bot
      and leq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
        | Bot, _ -> true
        | _, Top -> true
        | Relation r1, Relation r2 -> leq_R r1 r2
        | Table t1, Table t2 -> leq_T t1 t2
        | Ary ary1, Ary ary2 -> leq_Ary ary1 ary2
        | Unit u1, Unit u2 -> true
        | _, _ -> false
      and eq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
      | Bot, Bot -> true
      | Top, Top -> true
      | Relation r1, Relation r2 -> eq_R r1 r2
      | Table t1, Table t2 -> eq_T t1 t2
      | Ary ary1, Ary ary2 -> eq_Ary ary1 ary2
      | Unit u1, Unit u2 -> true
      | _, _ -> false
      and is_table (v:value_t) = match v with
        | Table _ -> true
        | _ -> false
      and is_bool_V (v:value_t) = match v with
        | Relation r -> is_bool_R r
        | _ -> false
      and is_Relation = function 
        | Relation _ -> true
        | _ -> false
      and is_Array = function
        | Ary _ -> true
        | _ -> false
      and arrow_V var (v:value_t) (v':value_t) = match v' with
        | Bot -> Bot
        | Top | Table _ | Unit _ -> v
        | Relation r2 -> (match v with
            | Table t -> Table (arrow_T var t v')
            | Relation r1 -> Relation (arrow_R var r1 r2)
            | Ary ary -> Ary (arrow_Ary var ary r2)
            | _ -> v)
        | Ary ary2 -> let (vars, r2) = ary2 in
          (match v with
          | Table t -> Table (arrow_T var t v')
          | Relation r1 -> Relation (arrow_R var r1 r2)
          | Ary ary -> Ary (arrow_Ary var ary r2)
          | _ -> v)
      and forget_V var v = match v with
        | Table t -> Table (forget_T var t)
        | Ary ary -> Ary (forget_Ary var ary)
        | Relation r -> Relation (forget_R var r)
        | _ -> v
      and equal_V v var = match v with
        | Relation r -> Relation (equal_R r var)
        | Table t -> Table (equal_T t var)
        | _ -> v
      and wid_V v1 v2 = match v1, v2 with
        | Relation r1, Relation r2 -> Relation (wid_R r1 r2)
        | Table t1, Table t2 -> Table (wid_T t1 t2)
        | Ary ary1, Ary ary2 -> Ary (wid_Ary ary1 ary2)
        | Unit u1, Unit u2 -> Unit u1
        | _, _ -> join_V v1 v2
      and op_V sl sr op v = match v with
        | Bot | Top -> Top
        | Relation r -> Relation (op_R sl sr op false r)
        | _ -> raise (Invalid_argument "Should be a relation type when using op_V")
      and bool_op_V op v1 v2 = match v1, v2 with
        | Bot, Relation _ -> if string_of_op op = "&&" then Bot else v2
        | Top, Relation _ -> if string_of_op op = "&&" then v2 else Top
        | Relation r1, Relation r2 -> Relation (bool_op_R op r1 r2)
        | Relation _, Bot -> if string_of_op op = "&&" then Bot else v1
        | Relation _, Top -> if string_of_op op = "&&" then v1 else Top
        | _, _ -> raise (Invalid_argument "Should be a relation type when using bool_op_V")
      and dx_T v = match v with
        | Table t -> dx_Ta t
        | _ -> raise (Invalid_argument "Should be a table when using dx_T")
      and io_T v = match v with
        | Table t -> io_Ta t
        | _ -> raise (Invalid_argument "Should be a table when using io_T")
    and is_Bot_V = function
      | Bot -> true
      | _ -> false
    and replace_V v var x = match v with
      | Table t -> Table (replace_T t var x)
      | Relation r -> Relation (replace_R r var x)
      | Ary ary -> Ary (replace_Ary ary var x)
      | _ -> v
    and extrac_bool_V v b = match v with
      | Relation r -> Relation (extrac_bool_R r b)
      | _ -> raise (Invalid_argument "Should be relation when split if statement")
    and stren_V v ae = match v,ae with
      | Bot, _ -> Bot
      | Relation r, Relation rae -> Relation (stren_R r rae)
      | Ary ary, Relation rae -> Ary (stren_Ary ary rae)
      | Table t, Relation rae -> Table t
      | Top, _ -> ae
      | _, Bot -> Bot
      | _, Top -> v
      | Unit _, _ -> v
      | _,_ -> raise (Invalid_argument "ae should not be a table")
    and der_V term v = match term, v with
      | _, Top | _, Bot -> v
      | Const (c,l), Relation r -> 
        let r' = (match c with
          | Integer i -> der_R ((string_of_int i) ^ "=" ^ (name_of_node l)) r
          | Boolean b -> r (*TODO:this case*))
        in
        Relation r'
      | Var (x, l), Relation r -> Relation (der_R (x ^ "=" ^ (name_of_node l)) r)
      | App (e1, e2, l), Relation r -> v |> der_V e1 |> der_V e2
      | Rec (f_opt, x, lx, e1, l), Table t -> v (*TODO:this case*)
      | Ite (e1, e2, e3, l, _), Relation r -> v |> der_V e1 |> der_V e2 |> der_V e3
      | BinOp (bop, e1, e2, l), Relation r -> 
        let expr = ((e1 |> loc |> name_of_node)^(string_of_op bop)^(e2 |> loc |> name_of_node) ^ "=" ^ (name_of_node l)) in
        let v' = Relation (der_R expr r) in
        v' |> der_V e1 |> der_V e2
      | _, _ -> raise (Invalid_argument "derived values match incorrectly")
    and proj_V v vars =
      match v with
      | Relation r -> Relation (proj_R r vars)
      | Table t -> Table (proj_T t vars)
      | Ary ary -> Ary (proj_Ary ary vars)
      | _ -> v
    and get_len_var_V = function
      | Ary ary -> get_len_var_Ary ary
      | _ -> raise (Invalid_argument "get length dep variable unsucessful")
    and opt_eq_V v1 v2 = match v1, v2 with
      | Bot, _ | _, Bot -> false
      | _, Top -> true
      | Relation r1, Relation r2 -> is_bot_R r1 = false && is_bot_R r2 = false && eq_R r1 r2
      | Table (z1, v1i, v1o), Table (z2, v2i, v2o) ->
          z1 = z2 && opt_eq_V v1i v2i && opt_eq_V v1o v2o
      | Ary ary1, Ary ary2 -> eq_Ary ary1 ary2
      | Unit u1, Unit u2 -> true
      | _, _ -> false 
    and is_bool_false_V = function
      | Relation r -> is_bool_false_R r
      | _ -> true
    (*
      *******************************
      ** Abstract domain for Array **
      *******************************
      *)
    and init_Ary vars = let var = Array.get vars 0 in
      let var' = if var = "l" then 
            let temp = var^(!l_index |> string_of_int) in
            incr_l();
            temp
          else var in
      let vars = [|var'|] in
      let r = bot_R Plus in
      vars, r
    and get_len_var_Ary ary = let vars, _ = ary in Array.get vars 0
    and empty_Ary l = Array.make l (top_R Plus)
    and join_Ary ary1 ary2 = 
      let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let vars1, r1 = ary1' in let vars2, r2 = ary2' in
      vars1, (join_R r1 r2)
    and meet_Ary ary1 ary2 = 
      let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let vars1, r1 = ary1' in let vars2, r2 = ary2' in
      vars1, (meet_R r1 r2)
    and leq_Ary ary1 ary2 = 
      let vars1, r1 = ary1 in let vars2, r2 = ary2 in
      let scop_check = Array.fold_left (fun b i -> if Array.mem i vars2 then b else false) true vars1 in
      if scop_check then leq_R r1 r2 else false
    and eq_Ary ary1 ary2 =
      let vars1, r1 = ary1 in let vars2, r2 = ary2 in
      let scop_check = Array.fold_left (fun b i -> if Array.mem i vars2 then b else false) true vars1 in
      if scop_check then eq_R r1 r2 else false
    and wid_Ary ary1 ary2 =
      let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let vars1, r1 = ary1' in let vars2, r2 = ary2' in
      vars1, (wid_R r1 r2)
    and arrow_Ary var ary r2 = let (vars, r) = ary in
      (vars, arrow_R var r r2)
    and forget_Ary var ary = let (vars, r) = ary in
      (vars, forget_R var r)
    and stren_Ary ary ae = let (vars, r) = ary in
      let r' = stren_R r ae in
      (vars, r')
    and proj_Ary ary vars = let (vs, r) = ary in
      let vars' = Array.append vars vs in
      (vs, proj_R r vars')
    and alpha_rename_Arys ary1 ary2 = 
      let vars1, r1 = ary1 in let vars2, r2 = ary2 in
      let p_ary2 = ref ary2 in
      let _ = Array.map2 (fun a b -> if a = b then b else 
        (p_ary2 := alpha_rename_Ary !p_ary2 b a; a)) vars1 vars2 in
      let ary2' = !p_ary2 in
      ary1, ary2'
    and alpha_rename_Ary ary prevar var = let (vars, r) = ary in
      let vars' = Array.map (fun a -> if a = prevar then var else a) vars in
      let r' = alpha_rename_R r prevar var in
      (vars', r')
    and replace_Ary ary var x = let (vars, r) = ary in
      let vars' = Array.map (fun a -> if a = var then x else a) vars in
      let r' = replace_R r var x in
      (vars', r')
  end
