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
     and table_t = var * value_t * value_t
     and exec_map_t = value_t NodeMap.t
     let rec alpha_rename_R a prevar var = match a with
          | Int v -> Int (AbstractValue.alpha_rename v prevar var)
          | Bool (vt, vf) -> Bool ((AbstractValue.alpha_rename vt prevar var), (AbstractValue.alpha_rename vf prevar var))
     and top_R = function
      | Plus | Mult | Div | Mod | Minus -> (Int AbstractValue.top)
      | Ge | Eq | Ne | Lt | Gt | Le -> (Bool (AbstractValue.top, AbstractValue.top))
     and is_bool_R a = match a with
         | Int _ -> false
         | Bool _ -> true
     and init_R_c (c:value) = match c with
         | Boolean true -> Bool (AbstractValue.init_c 1, AbstractValue.init_c 0)
         | Boolean false -> Bool (AbstractValue.init_c 0, AbstractValue.init_c 1)
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
     and op_R l r op a = 
       match op with
       | Plus | Mult | Div | Mod | Minus -> (match a with
          | Int v -> Int (AbstractValue.operator l r op v)
          | Bool (vt, vf) -> raise (Invalid_argument "Conditional value given, expect arithmetic one"))
       | Ge | Eq | Ne | Lt | Gt | Le -> (match a with
          | Int v -> Bool (AbstractValue.operator l r op v, AbstractValue.operator l r (rev_op op) v)
          | Bool (vt, vf) -> Bool (AbstractValue.operator l r op vt, AbstractValue.operator l r (rev_op op) vf))
     and replace_R a var x = alpha_rename_R a var x
     and extrac_bool_R v b = match v,b with
      | Bool (vt, _), true -> Int vt
      | Bool (_, vf), false -> Int vf
      | _,_ -> raise (Invalid_argument "Extract abstract value for condition, expect bool one")
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
       (*
        *******************************
        ** Abstract domain for Values **
        *******************************
        *)
       and alpha_rename_V v prevar var = match v with
         | Bot -> Bot
         | Relation r -> Relation (alpha_rename_R r prevar var)
         | Table t -> Table (alpha_rename_T t prevar var)
         | Top -> Top
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
         | _, _ -> Top
       and meet_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
         | Top, v | v, Top -> v
         | Relation r1, Relation r2 -> Relation (meet_R r1 r2)
         | Table t1, Table t2 -> Table (meet_T t1 t2)
         | _, _ -> Bot
       and leq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
         | Bot, _ -> true
         | _, Top -> true
         | Relation r1, Relation r2 -> leq_R r1 r2
         | Table t1, Table t2 -> leq_T t1 t2
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
         | Top | Table _ -> v
         | Relation r2 -> (match v with
             | Bot | Top -> v
             | Table t -> Table (arrow_T var t v')
             | Relation r1 -> Relation (arrow_R var r1 r2))
       and forget_V var v = match v with
         | Bot | Top -> v
         | Table t -> Table (forget_T var t)
         | Relation r -> Relation (forget_R var r)
       and equal_V v var = match v with
         | Relation r -> Relation (equal_R r var)
         | Table t -> Table (equal_T t var)
         | _ -> v
       and wid_V v1 v2 = match v1, v2 with
         | Relation r1, Relation r2 -> Relation (wid_R r1 r2)
         | Table t1, Table t2 -> Table (wid_T t1 t2)
         | _, _ -> join_V v1 v2
       and op_V sl sr op v = match v with
         | Bot | Top -> Top
         | Relation r -> Relation (op_R sl sr op r)
         | Table t -> raise (Invalid_argument "Should be a relation type when using op_V")
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
        | Bot | Top -> v
        | Table t -> Table (replace_T t var x)
        | Relation r -> Relation (replace_R r var x)
      and extrac_bool_V v b = match v with
        | Relation r -> Relation (extrac_bool_R r b)
        | _ -> raise (Invalid_argument "Should be relation when split if statement")
   end
 
 (** Pretty printing *)
 let pr_relation ppf a = let open SemanticsDomain in match a with
 | Bool (vt, vf) -> Format.fprintf ppf "@[<1>{@ cur_v:@ Bool@ |@ TRUE:@ %a,@ FALSE:@ %a@ }@]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
 | (Int v) -> Format.fprintf ppf "@[<1>{@ cur_v:@ Int@ |@ %a@ }@]" AbstractValue.print_abs v
 
 let pr_label pl ppf l =
 if pl then Format.fprintf ppf "^%s" (string_of_int l) else ()
 
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
 
 let pr_node ppf n = Format.fprintf ppf "%s" (string_of_int @@ loc_of_node n)
 
 let rec pr_node_full ppf = function
 | EN (env, l) -> Format.fprintf ppf "@[<1><[%a], %s>@]" pr_env (VarMap.bindings env) (string_of_int l)
 
 and pr_env ppf = function
 | [] -> ()
 | [x, n] -> Format.fprintf ppf "%s: %a" x pr_node_full n
 | (x, n) :: env -> Format.fprintf ppf "%s: %a,@ %a" x pr_node_full n pr_env env
 
 let string_of_node n = pr_node Format.str_formatter n; Format.flush_str_formatter ()
 
 let rec pr_value ppf v = let open SemanticsDomain in match v with
 | Bot -> Format.fprintf ppf "_|_"
 | Top -> Format.fprintf ppf "T"
 | Relation r -> pr_relation ppf r
 | Table t -> pr_table ppf t
 
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
 