open AbstractDomain
open Syntax
open Util
  (*
   ****************************************
   ** Abstract domain for all semantics **
   ****************************************
   *)
   module VarMap = Map.Make(struct
    type t = var
    let compare = compare
 end)
 
 type baseType = Int | Bool
 type node_t = EN of env_t * loc (*N = E x loc*)
 and env_t = node_t VarMap.t (*E = Var -> N*)

 let name_of_node lb = ("lab_" ^ lb)
 
 module NodeMap = Map.Make(struct
    type t = node_t
    let compare = compare
 end)
 
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
       | Ary of relation_t (* Future plan: * (relation_t array)*)
       | Unit of unit
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
         | Boolean false -> Bool (AbstractValue.bot, AbstractValue.init_c 1)
         | Integer i -> Int (AbstractValue.init_c i)
     and init_R_b b = init_R_c (Boolean b)
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
     and arrow_R var a1 a2 = let a2' = alpha_rename_R a2 "cur_v" var in
          match a1, a2' with
          | Int _, Int _ -> meet_R a1 a2'
          | Bool _, Bool _ -> meet_R a1 a2'
          | Int _, Bool (vt, vf) -> join_R (meet_R a1 (Int vt)) (meet_R a1 (Int vf)) (* {v:int| a^at V a^af} *)
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
          | Int v -> Int (AbstractValue.operator l r op v)
          | Bool (vt, vf) -> raise (Invalid_argument "Conditional value given, expect arithmetic one"))
       | Ge | Eq | Ne | Lt | Gt | Le -> if cons then
          (match a with
          | Int v -> Int (AbstractValue.operator l r op v)
          | Bool (vt, vf) -> Bool (AbstractValue.operator l r op vt, AbstractValue.operator l r (rev_op op) vf)
          )
          else
          (match a with
          | Int v -> Bool (AbstractValue.operator l r op v, AbstractValue.operator l r (rev_op op) v)
          | Bool (vt, vf) -> Bool (AbstractValue.operator l r op vt, AbstractValue.operator l r (rev_op op) vf))
       | And | Or -> (match a with
          | Int v -> raise (Invalid_argument "Arithmetic value given, expect conditional one")
          | Bool (vt, vf) -> (*TODO: Implement this*) raise (Invalid_argument "Bool op not implement yet")
        )
     and replace_R a var x = alpha_rename_R a var x
     and extrac_bool_R v b = match v,b with
      | Bool (vt, _), true -> Int vt
      | Bool (_, vf), false -> Int vf
      | _,_ -> raise (Invalid_argument "Extract abstract value for condition, expect bool one")
     and stren_R a ae = match a, ae with
      | Int v, Int vae -> Int (AbstractValue.meet v vae)
      | Bool (vt, vf), Int vae -> Bool (AbstractValue.meet vt vae, AbstractValue.meet vf vae)
      | _, _ -> raise (Invalid_argument "ae should be {v:Int}")
    and der_R exp a = match a with
      | Bool (vt, vf) -> Bool (AbstractValue.derived exp vt, AbstractValue.derived exp vf)
      | Int v -> Int (AbstractValue.derived exp v)
    and proj_R a vars = match a with
      | Int v -> Int (AbstractValue.project_other_vars v vars)
      | Bool (vt, vf) -> Bool (AbstractValue.project_other_vars vt vars, AbstractValue.project_other_vars vf vars)
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
          let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
          in ("z", v1i, v1o'), ("z", v2i, v2o')
      and join_T t1 t2 = let t =
         let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
         if z1 = z2 then (z1, join_V v1i v2i, join_V v1o v2o) else (*a renaming*)
             let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
             in ("z", join_V v1i v2i, join_V v1o' v2o')
         in t
      and meet_T t1 t2 = let t =
          let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
          if z1 = z2 then (z1, meet_V v1i v2i, meet_V v1o v2o) else (*a renaming*)
              let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
              in ("z", meet_V v1i v2i, meet_V v1o' v2o')
          in t
      and leq_T t1 t2 = match t1, t2 with
         | (z1, v1i, v1o), (z2, v2i, v2o) -> if z1 = z2 then (leq_V v1i v2i) && (leq_V v1o v2o) else false
      and forget_T var t = let (z, vi, vo) = t in (z, forget_V var vi, forget_V var vo)
      and arrow_T var t v =
         let (z, vi, vo) = t in
         let vo' = forget_V z vo in
         (z, arrow_V var vi v, arrow_V var vo' v)
      and wid_T t1 t2 = let t =
         let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
         if z1 = z2 then (z1, wid_V v1i v2i, wid_V v1o v2o) else (*a renaming*)
          let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
          in ("z", wid_V v1i v2i, wid_V v1o' v2o') in t
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
        (z, proj_V vi vars, proj_V vo vars)
      (*
       ***************************************
       ** Abstract domain for Execution Map **
       ***************************************
       *)
       and meet_M m1 m2 =
         NodeMap.union (fun n v1 v2 -> Some (meet_V v1 v2)) m1 m2
       and join_M m1 m2 =
         NodeMap.union (fun n v1 v2 -> Some (join_V v1 v2)) m1 m2
       and leq_M m1 m2 =
         NodeMap.for_all (fun n v1 (*untie to node -> value*) ->
         NodeMap.find_opt n m2 |> Opt.map (fun v2 -> leq_V v1 v2) |>
         Opt.get_or_else (v1 = Bot)) m1
       and top_M m = NodeMap.map (fun a -> Top) m
       and array_M env m = 
          let n_make = EN (env, "make") in
          let t_make = 
            (* zm: {v:int | v >= 0} -> ex: {v:int | top} -> {v: int array (l) | l = zm ^ zm >= 0} *)
            let var_l = "zm" in
            let rl = top_R Plus |> op_R "cur_v" "0" Ge true in
            let var_e = "ex" in
            let rm = top_R Plus in (*TODO: Make poly*)
            let rlen = arrow_R var_l rm rl |> op_R "l" var_l Eq true in
            Table (var_l, Relation rl, Table (var_e, Relation rm, Ary rlen))
          in
          let n_len = EN (env, "len") in
          let t_len = (* zl: {v: int array (l) | l >= 0} -> {v: int | v = l} *)
            let var_l = "zl" in
            let var_len = "l" in
            let rl = replace_R (top_R Plus |> op_R "cur_v" "0" Ge true) "cur_v" var_len in
            let rlen = equal_R (top_R Plus) "l" in
            Table (var_l, Ary rl, Relation rlen)
          in
          let n_get = EN (env, "get") in
          let t_get = 
            (* zg: {v: int array (l) | l >= 0 } -> i: {v: int | 0 <= v < l} -> {v: int | top} *)
            let var_l = "zg" in
            let var_len = "l" in
            let rl = replace_R (top_R Plus |> op_R "cur_v" "0" Ge true) "cur_v" var_len in
            let var_i = "i" in
            let rm = top_R Plus |> op_R "cur_v" "0" Ge true |> op_R "cur_v" var_len Lt true in
            let rr = top_R Plus in
            Table (var_l, Ary rl, Table (var_i, Relation rm, Relation rr))
          in
          let n_set = EN (env, "set") in
          let t_set = 
            (* zs: {v: int array (l) | l >= 0 } -> i: {v: int | 0 <= v < l} -> ex: {v: int | top} -> unit *)
            let var_l = "zs" in
            let var_len = "l" in
            let rl = replace_R (top_R Plus |> op_R "cur_v" "0" Ge true) "cur_v" var_len in
            let var_i = "i" in
            let rm = top_R Plus |> op_R "cur_v" "0" Ge true |> op_R "cur_v" var_len Lt true in
            let var_e = "ex" in
            let rri = top_R Plus in
            let rrr = () in
            Table (var_l, Ary rl, Table (var_i, Relation rm, Table (var_e, Relation rri, Unit rrr)))
          in
          let m' = 
            m |> NodeMap.add n_make t_make |> NodeMap.add n_len t_len |> NodeMap.add n_get t_get
            |> NodeMap.add n_set t_set
          in
          let env' = 
            env |> VarMap.add "make" n_make |> VarMap.add "len" n_len |> VarMap.add "get" n_get
            |> VarMap.add "set" n_set
          in
          env', m'
       (*
        ********************************
        ** Abstract domain for Values **
        ********************************
        *)
       and alpha_rename_V v prevar var = match v with
         | Relation r -> Relation (alpha_rename_R r prevar var)
         | Table t -> Table (alpha_rename_T t prevar var)
         | Ary (l) -> Ary (alpha_rename_R l prevar var)
         | _ -> v
       and init_V_c (c:value) = Relation (init_R_c c)
       and c_V v1 v2 label = match v2 with
         | Relation r -> (match v1 with
           | Relation r' -> let tempr = alpha_rename_R r "cur_v" label in
           (try Relation (meet_R r' tempr)
           with Invalid_argument s -> Relation r')
           | _ -> v1)
         | _ -> v1
       and init_V_b b = Relation (init_R_b b)
       and join_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
         | Bot, v | v, Bot -> v
         | Relation r1, Relation r2 -> Relation (join_R r1 r2)
         | Table t1, Table t2 -> Table (join_T t1 t2)
         | Ary (l1), Ary (l2) -> Ary (join_R l1 l2)
         | Unit u1, Unit u2 -> Unit u1
         | _, _ -> Top
       and meet_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
         | Top, v | v, Top -> v
         | Relation r1, Relation r2 -> Relation (meet_R r1 r2)
         | Table t1, Table t2 -> Table (meet_T t1 t2)
         | Ary (l1), Ary (l2) -> Ary (meet_R l1 l2)
         | Unit u1, Unit u2 -> Unit u1
         | _, _ -> Bot
       and leq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
         | Bot, _ -> true
         | _, Top -> true
         | Relation r1, Relation r2 -> leq_R r1 r2
         | Table t1, Table t2 -> leq_T t1 t2
         | Ary (l1), Ary (l2) -> leq_R l1 l2
         | Unit u1, Unit u2 -> true
         | _, _ -> false
       and is_table v = match v with
         | Table _ -> true
         | _ -> false
       and is_bool_V v = match v with
         | Relation r -> is_bool_R r
         | _ -> false
       and is_Relation = function 
         | Relation _ -> true
         | _ -> false
       and arrow_V var v v' = match v' with
         | Bot -> Bot
         | Top | Table _ | Unit _ -> v
         | Relation r2 | Ary r2 -> (match v with
             | Table t -> Table (arrow_T var t v')
             | Relation r1 -> Relation (arrow_R var r1 r2)
             | Ary l -> Ary (arrow_R var l r2)
             | _ -> v)
       and forget_V var v = match v with
         | Table t -> Table (forget_T var t)
         | Ary (l) -> Ary (forget_R var l)
         | Relation r -> Relation (forget_R var r)
         | _ -> v
       and equal_V v var = match v with
         | Relation r -> Relation (equal_R r var)
         | Table t -> Table (equal_T t var)
         | _ -> v
       and wid_V v1 v2 = match v1, v2 with
         | Relation r1, Relation r2 -> Relation (wid_R r1 r2)
         | Table t1, Table t2 -> Table (wid_T t1 t2)
         | Ary (l1), Ary (l2) -> Ary (wid_R l1 l2)
         | Unit u1, Unit u2 -> Unit u1
         | _, _ -> join_V v1 v2
       and op_V sl sr op v = match v with
         | Bot | Top -> Top
         | Relation r -> Relation (op_R sl sr op false r)
         | _ -> raise (Invalid_argument "Should be a relation type when using op_V")
       and dx_T v = match v with
         | Table t -> dx_Ta t
         | _ -> raise (Invalid_argument "Should be table when using dx_T")
       and io_T v = match v with
         | Table t -> io_Ta t
         | _ -> raise (Invalid_argument "Should be table when using io_T")
      and is_Bot_V = function
        | Bot -> true
        | _ -> false
      and replace_V v var x = match v with
        | Table t -> Table (replace_T t var x)
        | Relation r -> Relation (replace_R r var x)
        | Ary (l) -> Ary (replace_R l var x)
        | _ -> v
      and extrac_bool_V v b = match v with
        | Relation r -> Relation (extrac_bool_R r b)
        | _ -> raise (Invalid_argument "Should be relation when split if statement")
      and stren_V v ae = match v,ae with
        | Bot, _ -> Bot
        | Relation r, Relation rae -> Relation (stren_R r rae)
        | Table t, Relation rae -> Table t
        | Top, _ -> ae
        | _, Bot -> Bot
        | _, Top -> v
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
        | Ite (e1, e2, e3, l), Relation r -> v |> der_V e1 |> der_V e2 |> der_V e3
        | BinOp (bop, e1, e2, l), Relation r -> 
          let expr = ((e1 |> loc |> name_of_node)^(string_of_op bop)^(e2 |> loc |> name_of_node) ^ "=" ^ (name_of_node l)) in
          let v' = Relation (der_R expr r) in
          v' |> der_V e1 |> der_V e2
        | _, _ -> raise (Invalid_argument "derived values match incorrectly")
      and proj_V v vars =
        match v with
        | Relation r -> Relation (proj_R r vars)
        | Table t -> Table (proj_T t vars)
        | Ary (l) -> Ary (proj_R l (vars |> Array.append [|"l"|]))
        | _ -> v
      (*
       *******************************
       ** Abstract domain for Array **
       *******************************
       *)
      and init_Ary l c = Array.make l (init_R_c c)
      and empty_Ary l = Array.make l (top_R Plus)
      and join_Ary ary1 ary2 = 
        try Array.map2 join_R ary1 ary2 with 
        Invalid_argument s -> raise (Invalid_argument ("Array join: " ^ s))
      and meet_Ary ary1 ary2 = try Array.map2 meet_R ary1 ary2 with
        Invalid_argument s -> raise (Invalid_argument ("Array meet: " ^ s))
      and leq_Ary ary1 ary2 = try Array.map2 leq_R ary1 ary2 |> Array.for_all (fun a -> a) with
        Invalid_argument s -> raise (Invalid_argument ("Array leq: " ^ s))
      and wid_Ary ary1 ary2 = try Array.map2 wid_R ary1 ary2 with
      Invalid_argument s -> raise (Invalid_argument ("Array wid: " ^ s))
      and forget_Ary var ary = Array.map (fun r -> forget_R var r) ary
      and proj_Ary ary vars = Array.map (fun r -> proj_R r vars) ary
      and alpha_rename_Ary ary prevar var = Array.map (fun r -> alpha_rename_R r prevar var) ary
      and replace_Ary ary var x = Array.map (fun r -> replace_R r var x) ary
   end
 
 (** Pretty printing *)
 let pr_relation ppf a = let open SemanticsDomain in match a with
 | Bool (vt, vf) -> Format.fprintf ppf "@[<1>{@ cur_v:@ Bool@ |@ TRUE:@ %a,@ FALSE:@ %a@ }@]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
 | (Int v) -> Format.fprintf ppf "@[<1>{@ cur_v:@ Int@ |@ %a@ }@]" AbstractValue.print_abs v
 
 let pr_label pl ppf l =
 if pl then Format.fprintf ppf "^%s" l else ()
 
 let pr_const ppf = function
   | Boolean b -> Format.fprintf ppf "%s" (string_of_bool b) 
   | Integer k -> Format.fprintf ppf "%s" (string_of_int k)
 
 let pr_op ppf op = Format.fprintf ppf "%s" (string_of_op op)
 
 let rec pr_exp pl ppf = function
 | Const (c, l) ->
     Format.fprintf ppf "%a%a" pr_const c (pr_label pl) l
 | Var (x, l) ->
     Format.fprintf ppf "%s%a" x (pr_label pl) l
 | App (e1, e2, l) ->
     Format.fprintf ppf "@[<2>(%a@ %a)%a@]"
       (pr_exp pl) e1
       (pr_exp pl) e2
       (pr_label pl) l
 | Rec (None, x, lx, e, l) ->
     Format.fprintf ppf "@[<3>(lambda %s%a.@ %a)%a@]"
       x (pr_label pl) lx
       (pr_exp pl) e
       (pr_label pl) l
 | Rec (Some (f, lf), x, lx, e, l) ->
     Format.fprintf ppf "@[<3>(mu %s%a %s%a.@ %a)%a@]"
       f (pr_label pl) lf
       x (pr_label pl) lx
       (pr_exp pl) e
       (pr_label pl) l
 | Ite (e1, e2, e3, l) ->
     Format.fprintf ppf "@[<2>(%a@ ?@ %a@ :@ %a)%a@]"
       (pr_exp pl) e1
       (pr_exp pl) e2
       (pr_exp pl) e3
       (pr_label pl) l
 | BinOp (bop, e1, e2, l) ->
     Format.fprintf ppf "@[<3>(%a@ %a@ %a)%a@]"
     (pr_exp pl) e1
     pr_op bop
     (pr_exp pl) e2
     (pr_label pl) l
 
 let print_exp out_ch e = Format.fprintf (Format.formatter_of_out_channel out_ch) "%a@?" (pr_exp true) e
 
 let loc_of_node = function
   | EN (_, l) -> l
 
 let pr_node ppf n = Format.fprintf ppf "%s" (loc_of_node n)
 
 let rec pr_node_full ppf = function
 | EN (env, l) -> Format.fprintf ppf "@[<1><[%a], %s>@]" pr_env (VarMap.bindings env) l
 
 and pr_env ppf = function
 | [] -> ()
 | [x, n] -> Format.fprintf ppf "%s: %a" x pr_node_full n
 | (x, n) :: env -> Format.fprintf ppf "%s: %a,@ %a" x pr_node_full n pr_env env
 
 let string_of_node n = pr_node Format.str_formatter n; Format.flush_str_formatter ()

 let pr_ary_val ppf a = let open SemanticsDomain in match a with
 | Bool (vt, vf) -> Format.fprintf ppf "@[<1>@ TRUE:@ %a,@ FALSE:@ %a@ @]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
 | (Int v) -> Format.fprintf ppf "@[<1>@ %a@ @]" AbstractValue.print_abs v

 let pr_ary ppf l = Format.fprintf ppf "@[<1>{@ cur_v:@ Int Array (l)@ |@ %a@ }@]" pr_ary_val l

 let pr_unit ppf u = Format.fprintf ppf "@[<1>unit@]"
 
 let rec pr_value ppf v = let open SemanticsDomain in match v with
 | Bot -> Format.fprintf ppf "_|_"
 | Top -> Format.fprintf ppf "T"
 | Relation r -> pr_relation ppf r
 | Table t -> pr_table ppf t
 | Unit u -> pr_unit ppf u
 | Ary (l) -> pr_ary ppf l
 
 and pr_table ppf t = let open SemanticsDomain in let (z, vi, vo) = t in
     Format.fprintf ppf "@[<2>%s: %a ->@ %a@]" z pr_value vi pr_value vo
 
 let print_value out_ch v = Format.fprintf (Format.formatter_of_out_channel out_ch) "%a@?" pr_value v
 
 let string_of_value v = pr_value Format.str_formatter v; Format.flush_str_formatter ()
 
 let rec pr_exec_map ppf m =
 Format.fprintf ppf "----\n%a----\n" pr_exec_rows (NodeMap.bindings m)
 and pr_exec_rows ppf = function
 | [] -> ()
 | [row] -> Format.fprintf ppf "%a\n" pr_exec_row row
 | row :: rows -> Format.fprintf ppf "%a@\n@\n%a" pr_exec_row row pr_exec_rows rows
 and pr_exec_row ppf (n, v) =
 Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]" pr_node n pr_value v
 
 let print_exec_map m = Format.fprintf Format.std_formatter "%a@?" pr_exec_map m
 
 let rec print_exps out_ch = function
  | [] -> ()
  | e :: tl -> print_exp out_ch e; print_exps out_ch tl