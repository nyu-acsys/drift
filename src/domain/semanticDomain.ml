open AbstractDomain
open Syntax
open Util
open SensitiveDomain
open SenSemantics

(*
**********************************
** Abstract Data Flow Semantics **
**********************************
*)

let name_of_node lb = ("z" ^ lb)

let fresh_length = fresh_func "l"

let fresh_item = fresh_func "e"

let pre_def_func = ref []

module SemanticsDomain =
  struct
    (*
    **********************************
    ** Abstract domain for Relation **
    **********************************
    *)
    let alpha_rename_R (a:relation_t) prevar var :relation_t = match a with
      | Int v -> Int (AbstractValue.alpha_rename v prevar var)
      | Bool (vt, vf) -> Bool ((AbstractValue.alpha_rename vt prevar var), (AbstractValue.alpha_rename vf prevar var))
      | Unit _ -> a
    let top_R = function
      | Plus | Mult | Div | Mod | Modc | Minus -> (Int AbstractValue.top)
      | Ge | Eq | Ne | Lt | Gt | Le | And | Or -> (Bool (AbstractValue.top, AbstractValue.top))
      | _ -> raise (Invalid_argument "top_R should use a relational type operator")
    let utop_R = function
      | UMinus -> (Int AbstractValue.top)
      | Not -> (Bool (AbstractValue.top, AbstractValue.top))
    let bot_R = function
      | Plus | Mult | Div | Mod | Modc | Minus -> (Int AbstractValue.bot)
      | Ge | Eq | Ne | Lt | Gt | Le | And | Or -> (Bool (AbstractValue.bot, AbstractValue.bot))
      | _ -> raise (Invalid_argument "bot_R should use a relational type operator")
    let is_bool_R a = match a with
      | Bool _ -> true
      | _ -> false
    let init_R_c (c:value) = match c with
      | Boolean true -> Bool (AbstractValue.from_int 1, AbstractValue.bot)
      | Boolean false -> Bool (AbstractValue.bot, AbstractValue.from_int 0)
      | Integer i -> Int (AbstractValue.from_int i)
      | UnitLit  -> Unit ()
      | IntList lst -> raise (Invalid_argument "This should be cover on the upper level")
    let join_R a1 a2 =
      match a1, a2 with
      | (Int v1), (Int v2) -> Int (AbstractValue.join v1 v2)
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> Bool (AbstractValue.join v1t v2t, AbstractValue.join v1f v2f)
      | Unit _, a | a, Unit _ -> a
      | Int v1, Bool (v2t, v2f) | Bool (v2t, v2f) , Int v1
        when AbstractValue.eq v2t AbstractValue.bot &&
          AbstractValue.eq v2f AbstractValue.bot
        -> Int v1
      | Int v1, Bool (v2t, v2f) | Bool (v2t, v2f) , Int v1
        when AbstractValue.eq v1 AbstractValue.bot
        -> Bool (v2t, v2f)          
      | _, _ ->          
          raise (Invalid_argument "Join: Base Type not equal")
    let meet_R a1 a2 =
      match a1, a2 with
      | (Int v1), (Int v2) -> Int  (AbstractValue.meet v1 v2)
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> Bool (AbstractValue.meet v1t v2t, AbstractValue.meet v1f v2f)
      | Unit _, _ | _, Unit _ -> Unit ()
      | _, _ -> raise (Invalid_argument "Meet: Base Type not equal")
    let leq_R a1 a2 =
      match a1, a2 with
      | (Int v1), (Int v2) -> AbstractValue.leq v1 v2
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> AbstractValue.leq v1t v2t && AbstractValue.leq v1f v2f
      | Unit u1, Unit u2 -> true
      | _, _ -> false
    let eq_R a1 a2 =
      match a1, a2 with
      | (Int v1), (Int v2) -> AbstractValue.eq v1 v2
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> AbstractValue.eq v1t v2t && AbstractValue.eq v1f v2f
      | Unit u1, Unit u2 -> true
      | _, _ -> false
    let arrow_R var a1 a2 = 
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
      | _, _ -> a1
    let forget_R var a = match a with
      | Int v -> Int (AbstractValue.forget_var var v)
      | Bool (vt, vf) -> Bool (AbstractValue.forget_var var vt, AbstractValue.forget_var var vf)
      | Unit _ -> a
    let equal_R a var = let eq_a = match a with
      | Int v -> Int (AbstractValue.equal_var v "cur_v" var)
      | Bool (vt, vf) -> Bool ((AbstractValue.equal_var vt "cur_v" var), (AbstractValue.equal_var vf "cur_v" var))
      | Unit _ -> a
      in
      eq_a
    let wid_R a1 a2 = match a1, a2 with
      | (Int v1), (Int v2) -> Int (AbstractValue.widening v1 v2)
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> Bool (AbstractValue.widening v1t v2t, AbstractValue.widening v1f v2f)
      | Unit u1, Unit u2 -> Unit ()
      | Unit _, a | a, Unit _ -> a
      | _, _ -> raise (Invalid_argument "Widening: Base Type not equal")
    let sat_equal_R a x = match a with
      | Int v -> AbstractValue.sat_cons v x
      | Bool (vt, vf) -> AbstractValue.sat_cons vt x && AbstractValue.sat_cons vf x
      | _ -> false
    let op_R res l r op cons a = (*cons for flag of linear constraints*)
      match op with
      | Plus | Mult | Div | Mod | Modc | Minus -> (match a with
        | Int v -> Int (AbstractValue.operator res l r op (-1) v)
        | _ -> raise (Invalid_argument "opR: Given a unit type"))
      | Ge | Eq | Ne | Lt | Gt | Le -> (if cons then
        (match a with
        | Int v -> Int (AbstractValue.operator res l r op (-1) v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator res l r op 1 vt, AbstractValue.operator res l r (rev_op op) 0 vf)
        | Unit _ -> raise (Invalid_argument "opR: Given a unit type")
        )
        else
        (match a with
        | Int v -> Bool (AbstractValue.operator res l r op 1 v, AbstractValue.operator res l r (rev_op op) 0 v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator res l r op 1 vt, AbstractValue.operator res l r (rev_op op) 0 vf)
        | Unit _ -> raise (Invalid_argument "opR: Given a unit type"))
      )
      | Cons | Seq | And | Or -> raise (Invalid_argument ("Invalid operator matched " ^ (string_of_op op)))
    let uop_R res op e cons a = (*cons for flag of linear constraints*)
      match op with
      | UMinus -> (match a with
        | Int v -> Int (AbstractValue.uoperator res e op (-1) v)
        | _ -> raise (Invalid_argument "uop_R: Given a unit type"))
      | Not -> failwith "unary negation not yet implemented"
    let assign_R res l r op = function 
      | Int v -> Int (AbstractValue.assign res l r op v)
      | _ -> raise (Invalid_argument "Assign boolean does not support")
    let bool_op_R op a1 a2 = match a1, a2 with
      | (Bool (v1t, v1f)), (Bool (v2t, v2f)) -> if string_of_op op = "&&" then
        Bool (AbstractValue.meet v1t v2t, AbstractValue.join v1f v2f)
      else
        Bool (AbstractValue.join v1t v2t, AbstractValue.meet v1f v2f)
      | _, _ -> raise (Invalid_argument "&& or || operation: Base Type should be bool")
    let replace_R a var x = alpha_rename_R a var x
    let extrac_bool_R v b = match v,b with
      | Bool (vt, _), true -> Int vt |> forget_R "cur_v"
      | Bool (_, vf), false -> Int vf |> forget_R "cur_v"
      | _,_ -> raise (Invalid_argument "Extract abstract value for condition, expect bool one")
    let stren_R a ae = 
      match a, ae with
      | Int v, Int vae -> Int (AbstractValue.meet v vae)
      | Bool (vt, vf), Int vae -> Bool (AbstractValue.meet vt vae, AbstractValue.meet vf vae)
      | Unit _, Int vae -> a
      | _, _ -> raise (Invalid_argument "ae should be {v:Int}")
    let der_R exp a = match a with
      | Bool (vt, vf) -> Bool (AbstractValue.derived exp vt, AbstractValue.derived exp vf)
      | Int v -> Int (AbstractValue.derived exp v)
      | Unit _ -> a
    let proj_R a vars = match a with
      | Int v -> Int (AbstractValue.project_other_vars v vars)
      | Bool (vt, vf) -> Bool (AbstractValue.project_other_vars vt vars, AbstractValue.project_other_vars vf vars)
      | Unit _ -> a
    let is_bot_R a = match a with
      | Int v -> AbstractValue.is_bot v
      | Bool (vt, vf) -> AbstractValue.is_bot vt && AbstractValue.is_bot vf
      | Unit _ -> false
    let is_bool_bot_R a = match a with
      | Bool (vt, vf) -> AbstractValue.is_bot vf && AbstractValue.is_bot vt
      | _ -> raise (Invalid_argument "Expect a bool value")
    let is_bool_false_R a = match a with
      | Bool (vt, vf) -> AbstractValue.is_bot vf && (AbstractValue.is_bot vt <> true)
      | _ -> raise (Invalid_argument "Expect a bool value")
    let opt_eq_R a1 a2 = is_bot_R a1 = false && is_bot_R a2 = false && eq_R a1 a2
    let contains_var_R var a = match a with
      | Int v -> AbstractValue.contains_var var v
      | Bool (vt, vf) -> AbstractValue.contains_var var vt && AbstractValue.contains_var var vf
      | Unit _ -> false
    let bot_shape_R = function
      | Int _ -> bot_R Plus
      | Bool _ -> bot_R Ge
      | Unit u as a -> a
    let is_unit_R = function
      | Unit _ -> true
      | _ -> false
      (*
      ********************************
      ** Abstract domain for Values **
      ********************************
      *)
    let rec alpha_rename_V v prevar var = match v with
        | Relation r -> Relation (alpha_rename_R r prevar var)
        | Table t -> Table (alpha_rename_T alpha_rename_V t prevar var)
        | Ary ary -> Ary (alpha_rename_Ary ary prevar var)
        | Lst lst -> Lst (alpha_rename_Lst lst prevar var)
        | _ -> v
      and init_V_c = function
        | IntList lst -> if List.length lst = 0 then Lst (init_Lst_c ())
          else Lst (const_Lst lst)
        | c -> Relation (init_R_c c)
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
        | Table t1, Table t2 -> Table (join_T join_V alpha_rename_V t1 t2)
        | Ary ary1, Ary ary2 -> Ary (join_Ary ary1 ary2)
        | Lst lst1, Lst lst2 -> Lst (join_Lst lst1 lst2)
        | Tuple u1, Tuple u2 -> Tuple (join_Tuple u1 u2)
        | _, _ -> Top
      and meet_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
        | Top, v | v, Top -> v
        | Relation r1, Relation r2 -> Relation (meet_R r1 r2)
        | Table t1, Table t2 -> Table (meet_T meet_V alpha_rename_V t1 t2)
        | Ary ary1, Ary ary2 -> Ary (meet_Ary ary1 ary2)
        | Lst lst1, Lst lst2 -> Lst (meet_Lst lst1 lst2)
        | Tuple u1, Tuple u2 -> Tuple (meet_Tuple u1 u2)
        | _, _ -> Bot
      and leq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
        | Bot, _ -> true
        | _, Top -> true
        | Relation r1, Relation r2 -> leq_R r1 r2
        | Table t1, Table t2 -> leq_T leq_V t1 t2
        | Ary ary1, Ary ary2 -> leq_Ary ary1 ary2
        | Lst lst1, Lst lst2 -> leq_Lst lst1 lst2
        | Tuple u1, Tuple u2 -> leq_Tuple u1 u2
        | _, _ -> false
      and sat_leq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
        | Bot, _ -> true
        | _, Top -> true
        | Relation r1, Relation r2 -> leq_R r1 r2
        | Table t1, Table t2 -> leq_T leq_V t1 t2
        | Ary ary1, Ary ary2 -> leq_Ary ary1 ary2
        | Lst lst1, Lst lst2 -> sat_leq_Lst lst1 lst2
        | Tuple u1, Tuple u2 -> leq_Tuple u1 u2
        | _, _ -> false
      and eq_V (v1:value_t) (v2:value_t) :bool = match v1, v2 with
        | Bot, Bot -> true
        | Top, Top -> true
        | Relation r1, Relation r2 -> eq_R r1 r2
        | Table t1, Table t2 -> eq_T eq_V t1 t2
        | Ary ary1, Ary ary2 -> eq_Ary ary1 ary2
        | Lst lst1, Lst lst2 -> eq_Lst lst1 lst2
        | Tuple u1, Tuple u2 -> eq_Tuple u1 u2
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
      and is_List = function
        | Lst _ -> true
        | _ -> false
      and arrow_V var (v:value_t) (v':value_t) = match v' with
        | Bot | Top | Table _ -> v
        | Relation r2 -> (match v with
            | Table t -> Table (arrow_T forget_V arrow_V var t v')
            | Relation r1 -> Relation (arrow_R var r1 r2)
            | Ary ary -> Ary (arrow_Ary var ary r2 None)
            | Lst lst -> Lst (arrow_Lst var lst (Relation r2) None)
            | Tuple u -> Tuple (arrow_Tuple var u v')
            | _ -> v)
        | Tuple u2 -> List.fold_left (fun v1 v2 ->
          arrow_V var v1 v2) v u2
        | Ary ary2 -> let (vars, (rl2, re2)) = ary2 in
          (match v with
          | Table t -> Table (arrow_T forget_V arrow_V var t v')
          | Relation r1 -> Relation (
            let r1' = arrow_R var r1 rl2 in
            if is_bot_R re2 then r1' else
            arrow_R var r1' re2
          )
          | Ary ary -> Ary (arrow_Ary var ary re2 (Some rl2))
          | Lst lst -> Lst (arrow_Lst var lst (Relation re2) (Some (vars, rl2)))
          | _ -> v)
        | Lst lst2 -> let ((l2,e2) as vars, (rl2, ve2)) = lst2 in
          (match v with
          | Table t -> Table (arrow_T forget_V arrow_V var t v')
          | Relation r1 -> if is_bot_R rl2 then Relation (bot_shape_R r1) else
            let r1' = let res = meet_R r1 rl2 in
            if is_bot_R res then (meet_R (forget_R l2 r1) rl2) else res in
            (match ve2 with
            | Bot -> if String.length var >= 2 && String.sub var 0 2 = "zh" then
                raise (Invalid_argument "List.hd expects non empty list")
              else Relation r1' 
            | Relation re2 -> if is_bot_R re2 then Relation r1'
              else Relation (meet_R r1' (proj_R re2 [e2]))
            | Tuple ue -> Relation r1'
            | _ -> arrow_V var (Relation r1') ve2)
          | Ary ary -> (match ve2 with
            | Relation re2 -> Ary (arrow_Ary var ary re2 (Some rl2))
            | _ -> Ary (arrow_Ary var ary rl2 None))
          | Lst lst -> Lst (arrow_Lst var lst ve2 (Some (vars, rl2)))
          | Top -> if String.sub var 0 2 = "zh" then
              (match ve2 with
                | Relation re2 -> 
                  let r1' = arrow_R var (equal_R (top_R Plus) e2) rl2 in
                  arrow_V var (Relation r1') ve2
                | _ -> ve2)
              else v
          | _ -> v)
      and forget_V var v = match v with
        | Table t -> Table (forget_T forget_V var t)
        | Ary ary -> Ary (forget_Ary var ary)
        | Lst lst -> Lst (forget_Lst var lst)
        | Relation r -> Relation (forget_R var r)
        | Tuple u -> Tuple (forget_Tuple var u)
        | _ -> v
      and equal_V v var = match v with
        | Relation r -> Relation (equal_R r var)
        | Table t -> Table (equal_T equal_V alpha_rename_V t var)
        | _ -> v
      and wid_V v1 v2 = match v1, v2 with
        | Relation r1, Relation r2 -> Relation (wid_R r1 r2)
        | Table t1, Table t2 -> Table (wid_T wid_V alpha_rename_V t1 t2)
        | Ary ary1, Ary ary2 -> Ary (wid_Ary ary1 ary2)
        | Lst lst1, Lst lst2 -> Lst (wid_Lst lst1 lst2)
        | Tuple u1, Tuple u2 -> Tuple (wid_Tuple u1 u2)
        | _, _ -> join_V v1 v2
      and op_V sl sr op v = match v with
        | Bot | Top -> Top
        | Relation r -> Relation (op_R "" sl sr op false r)
        | _ -> raise (Invalid_argument "Should be a relation type when using op_V")
      and uop_V op s v = match v with
        | Bot | Top -> Top
        | Relation r -> Relation (uop_R "" op s false r)
        | _ -> raise (Invalid_argument "Should be a relation type when using uop_V")
      and bool_op_V op v1 v2 = match v1, v2 with
        | Bot, Relation _ -> if string_of_op op = "&&" then Bot else v2
        | Top, Relation _ -> if string_of_op op = "&&" then v2 else Top
        | Relation r1, Relation r2 -> Relation (bool_op_R op r1 r2)
        | Relation _, Bot -> if string_of_op op = "&&" then Bot else v1
        | Relation _, Top -> if string_of_op op = "&&" then v1 else Top
        | _, _ -> raise (Invalid_argument "Should be a relation type when using bool_op_V")
    and is_Bot_V = function
      | Bot -> true
      | _ -> false
    and replace_V v var x = match v with
      | Table t -> Table (replace_T replace_V t var x)
      | Relation r -> Relation (replace_R r var x)
      | Ary ary -> Ary (replace_Ary ary var x)
      | Lst lst -> Lst (replace_Lst lst var x)
      | _ -> v
    and extrac_bool_V v b = match v with
      | Relation r -> Relation (extrac_bool_R r b)
      | _ -> raise (Invalid_argument "Should be relation when split if statement")
    and stren_V v ae = match v,ae with
      | Bot, _ -> Bot
      | Relation r, Relation rae -> if is_bot_R rae && is_unit_R r then Bot else
        Relation (stren_R r rae)
      | Ary ary, Relation rae -> Ary (stren_Ary ary rae)
      | Lst lst, Relation rae -> Lst (stren_Lst lst rae)
      | Table t, Relation rae -> Table t
      | Tuple u, Relation rae -> Tuple (stren_Tuple u ae)
      | Top, _ -> ae
      | _, Bot -> Bot
      | _, Top -> v
      | _,_ -> raise (Invalid_argument "ae should not be a table")
    and sat_equal_V v x = match v with
      | Relation r -> sat_equal_R r x
      | _ -> false
    (* and der_V term v = match term, v with
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
      | _, _ -> raise (Invalid_argument "derived values match incorrectly") *)
    and proj_V v vars =
      match v with
      | Relation r -> Relation (proj_R r vars)
      | Table t -> Table (proj_T proj_V get_list_length_item_V t vars)
      | Ary ary -> Ary (proj_Ary ary vars)
      | Lst lst -> Lst (proj_Lst lst vars)
      | Tuple u -> Tuple (proj_Tuple u vars)
      | _ -> v
    and get_len_var_V = function
      | Ary ary -> get_len_var_Ary ary
      | Lst lst -> get_len_var_Lst lst
      | _ -> raise (Invalid_argument "get length dep variable unsucessful")
    and get_item_var_V = function
      | Ary ary -> get_item_var_Ary ary
      | Lst lst -> get_item_var_Lst lst
      | _ -> raise (Invalid_argument "get item dep variable unsucessful")
    (* and opt_eq_V v1 v2 = match v1, v2 with
      | Bot, _ | _, Bot -> false
      | _, Top -> true
      | Relation r1, Relation r2 -> is_bot_R r1 = false && is_bot_R r2 = false && eq_R r1 r2
      | Table (z1, v1i, v1o), Table (z2, v2i, v2o) ->
          z1 = z2 && opt_eq_V v1i v2i && opt_eq_V v1o v2o
      | Ary ary1, Ary ary2 -> eq_Ary ary1 ary2
      | Unit u1, Unit u2 -> true
      | _, _ -> false  *)
    and is_bool_bot_V = function
      | Relation r -> is_bool_bot_R r
      | _ -> true
    and is_bool_false_V = function
      | Relation r -> is_bool_false_R r
      | _ -> true
    and get_second_table_input_V = function
      | Bot -> Bot
      | Table t -> if table_isempty t then Bot else
        let _,(_,vo) = get_full_table_T t in
        (match vo with
          | Bot -> Bot
          | Table t -> if table_isempty t then Bot else
            let _,(vi,_) = get_full_table_T t in vi
          | _ -> raise (Invalid_argument "array.set should be a table"))
      | _ -> raise (Invalid_argument "array.set should be a table")
    and join_for_item_V v1 v2 = match v1, v2 with
      | Bot, _ | _, Bot -> v1
      | Ary ary, Relation r -> Ary (join_for_item_Ary ary r)
      | _,_ -> raise (Invalid_argument "Approximate array's item should use relation type")
    and extrac_item_V vars = function
      | Bot | Top as v -> v
      | Lst lst -> extrac_item_Lst vars lst
      | _ -> raise (Invalid_argument "item inside either a list or an array")
    and reduce_len_V len le_lst = function
      | Bot | Top as v -> v
      | Lst lst -> Lst (reduce_len_Lst len le_lst lst)
      | _ -> raise (Invalid_argument "reduce length either a list or an array")
    and list_cons_V f v1 v2 = match v1, v2 with
      | Bot, _ | _, Bot -> Bot
      | v, Lst lst -> Lst (list_cons_Lst f v lst)
      | _,_ -> raise (Invalid_argument "List construct should be item :: lst")
    and alpha_rename_Vs v1 v2 = match v1, v2 with
      | Lst (((l1,e1), (rl1,ve1)) as lst1), Lst (((l2,e2), (rl2,ve2)) as lst2) ->
        if l1 = l2 && e1 = e2 then Lst lst1, Lst lst2 else
        let lst2 = 
          ((l2,e2), (rl2 |> forget_R l1 |> forget_R e1,
          ve2 |> forget_V l1 |> forget_V e1)) in
        let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
        Lst lst1', Lst lst2'
      | Ary ary1, Ary ary2 -> let ary1', ary2' =  alpha_rename_Arys ary1 ary2 in
        Ary ary1', Ary ary2'
      | _, _ -> v1, v2
    and bot_relation_V (tp: inputType) = match tp with
      | Int -> Relation (bot_R Plus)
      | Bool -> Relation (bot_R Ge)
      | Unit -> Tuple []
    and get_list_length_item_V = function
      | Lst lst -> get_list_length_item_Lst lst
      | _ -> []
    and only_shape_V = function
      | Relation r -> is_bot_R r
      | Lst lst -> only_shape_Lst lst
      | Ary ary -> only_shape_Ary ary
      | _ -> false
    and cons_temp_lst_V t = function
      | Lst lst -> Lst (cons_temp_lst_Lst t lst)
      | _ -> raise (Invalid_argument "Temp list construct should be item :: lst")
    and item_shape_V te t = match te, t with
      | Lst lste, Lst lst ->
        Lst (item_shape_Lst lste lst)
      | _, _ -> t
    and bot_shape_V = function
      | Bot | Top -> Bot
      | Relation r -> Relation (bot_shape_R r)
      | Table t -> Table (bot_shape_T bot_shape_V t)
      | Lst lst -> Lst (bot_shape_Lst lst)
      | Ary ary -> Ary (bot_shape_Ary ary)
      | Tuple u -> Tuple (bot_shape_Tuple u)
    and add_tuple_item_V v v' = match v with
      | Tuple u -> Tuple (add_tuple_item_Tuple u v')
      | _ -> raise (Invalid_argument "Tuple constrct should be value, tuple")
    and is_tuple_V = function
      | Tuple u -> true
      | _ -> false
    and get_tuple_list_V = function
      | Tuple u -> get_tuple_list u
      | _ -> raise (Invalid_argument "extract tuple should be a tuple")
    and pattern_empty_lst_V = function
      | Lst lst -> Lst (pattern_empty_Lst lst)
      | _ -> raise (Invalid_argument "pattern x::[] should give a list")
    (*
      *******************************
      ** Abstract domain for Array **
      *******************************
      *)
    and init_Ary_c (): array_t = let varl, vare = fresh_length (), fresh_item () in
      let r1 = top_R Plus |> op_R "" varl "0" Eq true in
      (varl, vare), (r1, r1)
    and init_Ary vars : array_t = let varl, vare = vars in
      let varl' = if varl = "l" || varl = "l'" then fresh_length () else varl in
      let vare' = if vare = "e" || vare = "e'" then fresh_item () else vare in
      let vars = varl', vare' in
      let r1 = bot_R Plus in
      vars, (r1, r1)
    and get_len_var_Ary ((varl,_),_) = varl
    and get_item_var_Ary ((_,vare),_) = vare
    and empty_Ary vars = Array.make vars (top_R Plus)
    and join_Ary ary1 ary2 = 
      let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let vars1, (rl1,re1) = ary1' in let vars2, (rl2,re2) = ary2' in
      vars1, (join_R rl1 rl2,join_R re1 re2)
    and meet_Ary ary1 ary2 = 
      let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let vars1, (rl1,re1) = ary1' in let vars2, (rl2,re2) = ary2' in
      vars1, (meet_R rl1 rl2, meet_R re1 re2)
    and leq_Ary ary1 ary2 = 
      let (l1,e1), (rl1,re1) = ary1 in let (l2,e2), (rl2,re2) = ary2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then leq_R rl1 rl2 && leq_R re1 re2 else false
    and eq_Ary ary1 ary2 =
      let (l1,e1), (rl1,re1) = ary1 in let (l2,e2), (rl2,re2) = ary2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then eq_R rl1 rl2 && eq_R re1 re2 else false
    and wid_Ary ary1 ary2 =
      let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let vars1, (rl1,re1) = ary1' in let vars2, (rl2,re2) = ary2' in
      vars1, (wid_R rl1 rl2,wid_R re1 re2)
    and arrow_Ary var ary r ropt = let ((l,e) as vars, (rl,re)) = ary in
      let r' = forget_R l r |> forget_R e in
      match ropt with
      | Some rl' -> 
        let rl' = forget_R l rl' |> forget_R e in
        (vars, (arrow_R var rl rl',arrow_R var re r'))
      | None -> (vars, (arrow_R var rl r,arrow_R var re r))
    and forget_Ary var ary = let (vars, (rl,re)) = ary in
      (vars, (forget_R var rl, forget_R var re))
    and stren_Ary ary ae = let ((l,e) as vars, (rl,re)) = ary in
      let ae' = (forget_R e ae |> forget_R l) in
      (vars, (stren_R rl ae', stren_R re ae'))
    and proj_Ary ary vars = let ((l,e), (rl,re)) = ary in
      let vars' = e :: l :: vars in
      ((l,e), (proj_R rl vars', proj_R re vars'))
    and alpha_rename_Arys ary1 ary2 = 
      let (l1,e1), (rl1,re1) = ary1 in let (l2,e2), (rl2,re2) = ary2 in
      let ary2' = match l1 = l2, e1 = e2 with
        | true, true -> ary2
        | false, true -> alpha_rename_Ary ary2 l2 l1
        | true, false -> alpha_rename_Ary ary2 e2 e1
        | false, false -> let ary2 = alpha_rename_Ary ary2 l2 l1 in alpha_rename_Ary ary2 e2 e1
      in
      ary1, ary2'
    and alpha_rename_Ary ary prevar var = let (l,e), (rl,re) = ary in
      let l' = if l = prevar then var else l in
      let e' = if e = prevar then var else e in
      let rl' = alpha_rename_R rl prevar var in
      let re' = alpha_rename_R re prevar var in
      (l',e'), (rl',re')
    and replace_Ary ary var x = let ((l,e), (rl,re)) = ary in
      let l' = if l = var then x else l in
      let e' = if e = var then x else e in
      let rl' = replace_R rl var x in
      let re' = replace_R re var x in
      (l',e'), (rl',re')
    and join_for_item_Ary ary r = 
      let ((l,e), (rl,re)) = ary in
      if is_bot_R rl && is_bot_R re then ary else
      let re' = alpha_rename_R r "cur_v" e |> join_R re in
      (l,e), (rl,re')
    and bot_shape_Ary (vars, (rl, re)) = 
      (vars, (rl, bot_shape_R re))
    and only_shape_Ary (vars, (rl, re)) = 
      is_bot_R rl && is_bot_R re 
    (*
      *******************************
      ** Abstract domain for List **
      *******************************
    *)
    and init_Lst_c (): list_t = let varl, vare = fresh_length (), fresh_item () in
      let r1 = top_R Plus |> op_R varl varl "0" Eq true in
      (varl, vare), (r1, Bot)
    and const_Lst lst = 
      let varl, vare = fresh_length (), fresh_item () in
      let min, max = List.fold_left (fun (min, max) item -> 
        let min' = if min > item then item else min in
        let max' = if max < item then item else max in
        min', max') (max_int, min_int) lst in
      let rl = top_R Plus |> op_R varl varl (string_of_int (List.length lst)) Eq true in
      let re = rl |> op_R vare vare (string_of_int min) Ge true 
        |> op_R vare vare (string_of_int max) Le true in
      (varl, vare), (rl, Relation re)
    and init_Lst vars : list_t = let varl, vare = vars in
      let varl' = if varl = "l" || varl = "l'" then fresh_length () else varl in
      let vare' = if vare = "e" || vare = "e'" then fresh_item () else vare in
      let vars = varl', vare' in
      vars, (bot_R Plus, Bot)
    and get_len_var_Lst ((varl,_),_) = varl
    and get_item_var_Lst ((_,vare),_) = vare
    and pattern_empty_Lst ((l,e) as vars, (_, ve)) = 
      let rl' = top_R Plus |> op_R l l "0" Eq true in
      let ve' = bot_shape_V ve in
      vars, (rl', ve')
    and extrac_item_Lst vars ((_,vare), (_, ve)) =
      (* let vars' = vare :: vars in *)
      let ve' = match ve with
      | Relation re -> alpha_rename_V ve vare "cur_v"
      | _ -> ve
        (* alpha_rename_R (proj_R re vars' |> forget_R "cur_v") vare "cur_v"  *)
      in
      ve'
    and join_Lst lst1 lst2 = 
      let (l1, e1), (rl1,ve1) = lst1 in
      let (l2, e2), (rl2,ve2) = lst2 in
      let lst1', lst2' = if l1 <> "l" && contains_var_R l1 rl2 then
        let a, b = alpha_rename_Lsts lst2 lst1 in 
        b, a
        else alpha_rename_Lsts lst1 lst2 in
      let (l1, e1) as vars1, (rl1,ve1) = lst1' in let (l2, e2) as vars2, (rl2,ve2) = lst2' in
      vars1, (join_R rl1 rl2, join_V ve1 ve2)
    and meet_Lst lst1 lst2 = 
      let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
      let vars1, (rl1,ve1) = lst1' in let vars2, (rl2,ve2) = lst2' in
      vars1, (meet_R rl1 rl2, meet_V ve1 ve2)
    and leq_Lst lst1 lst2 = 
      let (l1,e1), (rl1,ve1) = lst1 in let (l2,e2), (rl2,ve2) = lst2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then leq_R rl1 rl2 && leq_V ve1 ve2 else false
    and sat_leq_Lst lst1 lst2 = 
      let (l1,e1), (rl1,ve1) = lst1 in let (l2,e2), (rl2,ve2) = lst2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then 
        let rl1 = proj_R rl1 [l1] in
        let rl2 = proj_R rl2 [l2] in
        leq_R rl1 rl2 && leq_V ve1 ve2
      else false
    and eq_Lst lst1 lst2 =
      let (l1,e1), (rl1,ve1) = lst1 in let (l2,e2), (rl2,ve2) = lst2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then eq_R rl1 rl2 && eq_V ve1 ve2 else false
    and wid_Lst lst1 lst2 =
      let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
      let vars1, (rl1,ve1) = lst1' in let vars2, (rl2,ve2) = lst2' in
      vars1, (wid_R rl1 rl2,wid_V ve1 ve2)
    and arrow_Lst var lst v ropt = 
      let ((l,e) as vars, (rl,ve)) = lst in
      match ropt with
      | Some ((_, e') , rl') -> (match v, ve with
        | Bot, _ -> if String.sub var 0 2 = "xs" && contains_var_R "zc" rl' then
           let rl = arrow_R var rl rl' in
           let re' = (op_R "" e "zc" Eq true rl) in
           (vars, (rl, Relation re'))
          else let rl = arrow_R var rl rl' in
           (vars, (rl, ve))
        | Relation r, Relation re ->
          if String.sub var 0 2 = "xs" then
            let rl = arrow_R var rl rl' in
            let re' = 
              if is_bot_R r then 
                let r' = (op_R "" e "zc" Eq true rl) |> meet_R re in
                r' 
              else 
                let r' = arrow_R var re r |> (op_R "" e e' Eq true) in
                let r'' = arrow_R var re r |> (op_R "" e "zc" Eq true) in
                join_R r' r''
            in
            (vars, (rl, Relation re'))
          else if String.sub var 0 2 = "zt" then
            let rl = arrow_R var rl rl' in
            let re' = arrow_R var re r |> (op_R "" e e' Eq true) in
            (vars, (rl, Relation re'))
          else 
            let rl' = forget_R l rl' |> forget_R e in
            let r' = forget_R l r |> forget_R e in
          (vars, (arrow_R var rl rl', Relation (arrow_R var re r')))
        | Relation r, _ -> 
          if String.sub var 0 2 = "xs" then
           let rl = arrow_R var rl rl' in
           let re' = 
            let r' = (op_R "" e e' Eq true rl) in
            let r'' = (op_R "" e "zc" Eq true rl) in
            join_R r' r''
           in
           (vars, (rl, Relation re'))
          else 
            let rl = arrow_R var rl rl' in
            let ve' = arrow_V var ve v in
            (vars, (rl, ve'))
        | _, _ -> 
          if String.sub var 0 2 = "xs" then
            let rl = arrow_R var rl rl' in
            let ve' = 
              if is_Bot_V v then 
              (* TODO: how could we get abstract value without change transformer *)
                ve 
              else 
                let v' = arrow_V var ve v in
                let v'' = arrow_V var ve v in
                join_V v' v''
            in
            (vars, (rl,  ve'))
          else if String.sub var 0 2 = "zt" then
            let rl = arrow_R var rl rl' in
            let ve' = v in
            (vars, (rl, ve'))
          else 
            let rl' = forget_R l rl' |> forget_R e in
            let v' = forget_V l v |> forget_V e in
          (vars, (arrow_R var rl rl', arrow_V var ve v'))
      )
      | None -> 
        match v, ve with
        | Relation r, Relation re ->
        let r' = forget_R l r |> forget_R e in
        (vars, (arrow_R var rl r', Relation (arrow_R var re r')))
        | Relation r, _ -> let r' = forget_R l r in
          (vars, (arrow_R var rl r',arrow_V var ve v))
        | _, _ -> vars, (rl,arrow_V var ve v)
    and forget_Lst var lst = let (vars, (rl,ve)) = lst in
      (vars, (forget_R var rl, forget_V var ve))
    and stren_Lst lst ae = let ((l,e) as vars, (rl,ve)) = lst in
      let ae' = (forget_R e ae |> forget_R l) in
      (vars, (stren_R rl ae', stren_V ve (Relation ae')))
    and proj_Lst lst vars = let ((l,e), (rl,ve)) = lst in
      let vars' = e :: l :: vars in
      ((l,e), (proj_R rl vars', proj_V ve vars'))
    and alpha_rename_Lsts lst1 lst2 = 
      let (l1,e1), _ = lst1 in let (l2,e2), _ = lst2 in
      let lst2' = match l1 = l2, e1 = e2 with
        | true, true -> lst2
        | false, true -> alpha_rename_Lst lst2 l2 l1
        | true, false -> alpha_rename_Lst lst2 e2 e1
        | false, false -> let lst2 = alpha_rename_Lst lst2 l2 l1 in alpha_rename_Lst lst2 e2 e1
      in
      lst1, lst2'
    and alpha_rename_Lst lst prevar var = let (l,e), (rl,ve) = lst in
      let l' = if l = prevar then var else l in
      let e' = if e = prevar then var else e in
      let rl' = alpha_rename_R rl prevar var in
      let ve' = alpha_rename_V ve prevar var in
      (l',e'), (rl',ve')
    and replace_Lst lst var x = let ((l,e), (rl,ve)) = lst in
      let l' = if l = var then x else l in
      let e' = if e = var then x else e in
      let rl' = replace_R rl var x in
      let ve' = replace_V ve var x in
      (l',e'), (rl',ve')
    and reduce_len_Lst len le_lst lst = let ((l,e), (rl,ve)) = lst in
      let l', e' = 
        match le_lst with
        | [] -> fresh_length (), fresh_item()
        | l' :: e' :: [] -> if l = l' then fresh_length (), fresh_item() else l', e'
        | _ -> raise (Invalid_argument "construct pattern tl should give [] or [l;e]")
       in
      let rl' = assign_R l' l (string_of_int len) Minus rl |> op_R "" l' "0" Ge true in
      let ve' = 
        match ve with
        | Relation re -> 
          let r' = (meet_R (forget_R l re) rl') in
          Relation (alpha_rename_R r' e e')
        | _ -> ve
      in
      (l',e'), (rl',ve')
    and list_cons_Lst f v lst = let ((l,e), (rl,ve)) = lst in
      if v = Bot || is_bot_R rl then (l,e), (bot_R Plus, Bot) else
      let rl' = assign_R l l "1" Plus rl in
      let ve' = match v, ve with
        | Relation r, Bot ->
          Relation (arrow_R e (rl') r)
        | Relation r, Relation re -> 
          let re' = arrow_R e rl' (forget_R l r) |> join_R (meet_R (forget_R l re) rl') in
          Relation re'
        | Table t, Bot -> let t' = init_T (dx_T v) in
          Table t'
        | Tuple u, Bot -> let u' = List.init (List.length u) (fun _ ->
             Bot) in Tuple u'
        | _ -> join_V v ve
      in
      (l,e), (rl',ve')
    and get_list_length_item_Lst ((l,e), _) = [l;e]
    and only_shape_Lst ((l,e), (rl,ve)) = 
      is_bot_R rl && ve = Bot
    and prop_Lst prop ((l1,e1) as vars1, (rl1,ve1)) ((l2,e2) as vars2, (rl2,ve2)) =
      let rl1', rl2' = rl1, join_R rl1 rl2 in
      let ve1', ve2' = match ve1, ve2 with
      | _, Bot | Bot, _ -> ve1, ve1
      | Relation re1, Relation re2 -> ve1, Relation (join_R re1 re2)
      | Table te1, Table te2 -> prop ve1 ve2
      | Lst lst1, Lst lst2 -> let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
        let lst1'', lst2'' = prop_Lst prop lst1' lst2' in
        let _, lst2'' = alpha_rename_Lsts lst2 lst2'' in
        Lst lst1'', Lst lst2''
      | Ary ary1, Ary ary2 -> Ary ary1, Ary (join_Ary ary1 ary2)
      | _, _ -> ve1, join_V ve1 ve2 in
      (vars1, (rl1', ve1')), (vars2, (rl2', ve2'))
    and cons_temp_lst_Lst v ((l,e) as vars, (rl,ve)) = 
      match v with
      | Relation r -> let re' = alpha_rename_R r "cur_v" e in
        (vars, (rl, Relation re'))
      | _ -> (vars, (rl, v))
    and item_shape_Lst (_, (_, vee)) (vars, (rl, ve)) =
      let ve' = bot_shape_V vee in
      (vars, (rl, ve'))
    and bot_shape_Lst (vars, (rl, ve)) = 
      (vars, (rl, bot_shape_V ve))
    (*
      *******************************
      ** Abstract domain for Tuple **
      *******************************
    *)
    and join_Tuple u1 u2 =
      List.map2 (fun v1 v2 -> join_V v1 v2) u1 u2
    and meet_Tuple u1 u2 =
      List.map2 (fun v1 v2 -> meet_V v1 v2) u1 u2
    and leq_Tuple u1 u2 =
      List.fold_left2 (fun b v1 v2 -> b && leq_V v1 v2) true u1 u2
    and eq_Tuple u1 u2 =
      List.fold_left2 (fun b v1 v2 -> b && eq_V v1 v2) true u1 u2
    and wid_Tuple u1 u2 =
      List.map2 (fun v1 v2 -> wid_V v1 v2) u1 u2
    and arrow_Tuple var u v' = 
      List.map (fun v -> arrow_V var v v') u
    and stren_Tuple u ae = 
      List.map (fun v -> stren_V v ae) u
    and add_tuple_item_Tuple u v =
      v :: u
    and proj_Tuple u vars = 
      List.map (fun v -> proj_V v vars) u
    and forget_Tuple var u =
      List.map (fun v -> forget_V var v) u
    and bot_shape_Tuple u = 
      List.map (fun v -> bot_shape_V v) u
    and get_tuple_list u = 
      u

    (*
      ***************************************
      ** Abstract domain for Execution Map **
      ***************************************
      *)
    let meet_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
      NodeMap.merge (fun n v1 v2 -> Some (meet_V v1 v2)) m1 m2
    let join_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
      NodeMap.union (fun n v1 v2 -> join_V v1 v2) m1 m2
    let wid_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
      NodeMap.union (fun n v1 v2 -> 
        (* ( 
          let l = get_label_snode n in
          if !debug then
          begin
          Format.printf "\n<=== Wid ===> %s\n" l;
          f Format.std_formatter v1;
          Format.printf "\n<<~~~~>> \n";
          f Format.std_formatter v2;
          Format.printf "\n";
          Format.printf "\nRES:\n";
          f Format.std_formatter res_v;
          Format.printf "\n";
          end
        ); *)
        wid_V v1 v2
        ) m1 m2
    let leq_M (m1: exec_map_t) (m2: exec_map_t) : bool =
      NodeMap.for_all (fun n v1 (*untie to node -> value*) -> 
        NodeMap.find_opt n m2 |> Opt.map (fun v2 -> leq_V v1 v2) |>
        Opt.get_or_else (v1 = Bot)) m1
    let eq_PM (m1:exec_map_t) (m2:exec_map_t) =
      NodeMap.for_all (fun n v1 (*untie to node -> value*) ->
        NodeMap.find_opt n m2 |> Opt.map (
        fun v2 -> 
          let l = get_label_snode n in
          if eq_V v1 v2 then true else 
          raise (Pre_Def_Change ("Predefined node changed at " ^ l))
         )
      |> Opt.get_or_else (v1 = v1)) m1
    let top_M m = NodeMap.map (fun a -> Top) m
    let array_M env m = 
      let n_make = construct_vnode env "Array.make" ("", "") in
      let s_make = construct_snode "" n_make in
      let t_make = (* make *)
        (* make |-> zm: {v:int | v >= 0} -> ex: {v:int | top} -> 
           {v: Int Array (l, e) | len: [| l=zm; zm>=0; |] item: [| l=zm; zm>=0; e = ex; |]} *)
        let var_l = "zm" in
        let rl = top_R Plus |> op_R "" "cur_v" "0" Ge true in
        let var_e = "ex" in
        let rm = top_R Plus in (*TODO: Make poly*)
        (* let ilen = op_R "i" "0" Ge true llen |> op_R "i" "l" Lt true in
           let rlen = op_R "x" var_e Eq true ilen in  *)
        let ary = 
          let l, e = "l", "e" in
          let rl = arrow_R var_l rm rl |> op_R "" l var_l Eq true in
          let re = arrow_R var_l rm rl |> op_R "" l var_l Eq true |> op_R "" e var_e Eq true in
          (l, e), (rl, re)
        in
        let t = 
          let t' = Table (construct_table (var_e, var_e) (Relation rm, Ary ary)) in
          Table (construct_table (var_l, var_l) (Relation rl, t')) in
        t
      in
      let n_len = construct_vnode env  "Array.length" ("", "") in
      let s_len = construct_snode "" n_len in
      let t_len = (* len *)
        (* len |-> zl: { v: Int Array (l, e) | len: [| l>=0; |] item: [| true; |] } 
           -> { v: Int | [| v=l |] } *)
        let var_l = "zl" in
        let var_len = "l" in
        (* let var_i = "i" in *)
        let ary = 
          let l, e = "l", "e" in
          let rl = top_R Plus |> op_R "" var_len "0" Ge true in
          let re = top_R Plus in
          (l, e), (rl, re)
          in
        (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
        let rlen = equal_R (top_R Plus) "l" in
        Table (construct_table (var_l, var_l) (Ary ary, Relation rlen))
      in
      let n_get = construct_vnode env "Array.get" ("", "") in
      let s_get = construct_snode "" n_get in
      let t_get = (* get *)
        (* get |-> zg: { v: Int Array (l, e) | l: [| l>=0; |] e: [| true; |] } 
            -> zi: {v: int | 0 <= v < l} -> {v: int | v = e; }  *)
        let var_l = "zg" in
        let var_len = "l" in
        (* let var_i = "i" in *)
        let ary = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" var_len in
          let re = top_R Plus in
          (l, e), (rl, re)
        in
        (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
        let rm = top_R Plus |> op_R "" "cur_v" "0" Ge true |> op_R "" "cur_v" var_len Lt true in
        let rr = top_R Plus |> op_R "" "cur_v" "e" Eq true in 
        let var_zi = "zi" in
        let t = 
          let t' = Table (construct_table (var_zi, var_zi) (Relation rm, Relation rr)) in
          Table (construct_table (var_l, var_l) (Ary ary, t')) in
        t
      in
      let n_set = construct_vnode env "Array.set" ("", "") in
      let s_set = construct_snode "" n_set in
      let t_set = (* set *)
        (* set |-> zs: { v: Int Array (l, e) | len: [| l>=0; |] item: [| true; |] } -> 
           zi: {v: int | 0 <= v < l} -> ex: {v: int | top } -> unit *)
        let var_l = "zs" in
        let var_len = "l" in
        (* let var_i = "i" in *)
        let ary = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" var_len in
          let re = top_R Plus in
          (l, e), (rl, re)
        in
        (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
        let rm = top_R Plus |> op_R "" "cur_v" "0" Ge true |> op_R "" "cur_v" var_len Lt true in
        let var_e = "ex" in
        let var_zi = "zi" in
        let rri = top_R Plus in
        let rrr = [] in
        let t = 
          let t' = Table (construct_table (var_e, var_e) (Relation rri, Tuple rrr)) in
          let t'' = Table (construct_table (var_zi, var_zi) (Relation rm, t')) in
          Table (construct_table (var_l, var_l) (Ary ary, t'')) in
        t
      in
      let m' = 
        m |> NodeMap.add s_make t_make |> NodeMap.add s_len t_len |> NodeMap.add s_get t_get
      |> NodeMap.add s_set t_set
      in
      let env' = 
        env |> VarMap.add "Array.make" (n_make, false) |> VarMap.add "Array.length" (n_len, false) |> VarMap.add "Array.get" (n_get, false)
      |> VarMap.add "Array.set" (n_set, false)
      in
      pre_def_func := List.append !pre_def_func ["Array.make"; "Array.length"; "Array.get"; "Array.set"];
      env', m'
    let list_M env m = 
      let n_len = construct_vnode env  "List.length" ("", "") in
      let s_len = construct_snode "" n_len in
      let t_len = (* len *)
        (* len |-> zl: { v: 'a List (l, e) | len: [| l>=0; |] item: true } 
           -> { v: Int | [| v=l |] } *)
        let var_l = "zl" in
        let var_len = "l" in
        (* let var_i = "i" in *)
        let list = 
          let l, e = "l", "e" in
          let rl = top_R Plus |> op_R "" var_len "0" Ge true in
          let ve = Top in
          (l, e), (rl, ve)
        in
        (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
        let rlen = equal_R (top_R Plus) "l" in
        Table (construct_table (var_l, var_l) (Lst list, Relation rlen))
      in
      let n_hd = construct_vnode env "List.hd" ("", "") in
      let s_hd = construct_snode "" n_hd in
      let t_hd = (* hd *)
        (* hd |-> zh: { v: 'a List (l, e) | l: [| l>=0; |] e: true } 
           -> true  *)
        let var_h = "zh" in
        (* let var_i = "i" in *)
        let list = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l in
          let ve = Top in
          (l, e), (rl, ve)
        in
        let tr = Top in
        let t =
          Table (construct_table (var_h, var_h) (Lst list, tr)) in
        t
      in
      let n_tl = construct_vnode env "List.tl" ("", "") in
      let s_tl = construct_snode "" n_tl in
      let t_tl = (* tl *)
        (* tl |-> zt: { v: Int List (l, e) | l: [| l>=0; |] e: true } -> 
           { v: Int List (l1, e1) | l1: [| l1=l-1; |] e1: e } *)
        let var_t = "zt" in
        let var_len, var_e = "l", "e" in
        (* let var_i = "i" in *)
        let list1 = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l in
          let ve = Top in
          (l, e), (rl, ve)
        in
        let list2 = 
          let l, e = "l'", "e'" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l |> op_R l var_len "1" Minus true in
          (* let re = replace_R (rl |> op_R "" "cur_v" var_e Eq true) "cur_v" e in *)
          let ve = Top in
          (l, e), (rl, ve)
        in
        let t =
          Table (construct_table (var_t, var_t) (Lst list1, Lst list2)) in
        t
      in
      let n_cons = construct_vnode env "List.cons" ("", "") in
      let s_cons = construct_snode "" n_cons in
      let t_cons = (* cons *)
        (* cons |-> zc: true -> xs: { v: Int List (l, e) | l: [| l>=0; |] e: true } -> 
           { v: Int List (l1, e1) | l': [| l1=l+1; |] e': e ⊔ zc |] }
         *)
        let var_c = "zc" in
        let ve = Top in
        let var_len, var_e = "l", "e" in
        (* let var_i = "i" in *)
        let list1 = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l in
          let ve = Top in
          (l, e), (rl, ve)
        in
        let var_l = "xs" in
        let list2 = 
          let l, e = "l'", "e'" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "1" Ge true) "cur_v" l |> op_R l var_len "1" Plus true in
          let ve = Top in
          (l, e), (rl, ve)
        in
        let t =
          let t' = Table (construct_table (var_l,var_l) (Lst list1, Lst list2)) in
          Table (construct_table (var_c, var_c) (ve, t')) in
        t
      in
      let m' = 
        m |> NodeMap.add s_hd t_hd |> NodeMap.add s_len t_len |> NodeMap.add s_tl t_tl
      |> NodeMap.add s_cons t_cons
      in
      let env' = 
        env |> VarMap.add "List.hd" (n_hd, false) |> VarMap.add "List.length" (n_len, false) |> VarMap.add "List.tl" (n_tl, false)
      |> VarMap.add "List.cons" (n_cons, false)
      in
      pre_def_func := List.append !pre_def_func ["List.hd"; "List.length"; "List.tl"; "List.cons"];
      env', m'
    let pref_M env m = 
      let m', env' =
        if VarDefMap.is_empty !pre_vars then
          m, env
        else 
          VarDefMap.fold (fun var (domain: pre_exp) (m, env) -> 
            let n_var = construct_vnode env var ("", "") in
            let s_var = construct_snode "" n_var in
            let t_var = match domain with
            | {name = n; dtype = Int; left = l; op = bop; right = r} -> 
                let rm = if l = "true" then 
                  (top_R Plus)
                else top_R Plus |> op_R "" l r bop true
                in Relation rm
            | {name = n; dtype = Bool; left = l; op = bop; right = r} -> 
                let rm = if l = "true" then init_R_c (Boolean true)
                else if l = "false" then
                  init_R_c (Boolean false)
                else
                  top_R Plus |> op_R "" l r bop true
                in
                Relation (rm)
            | {name = n; dtype = Unit; left = l; op = _; right = r} ->
                if l = "unit" then Tuple []
                else raise (Invalid_argument"Expected unit predicate as {v: Unit | unit }")
            in
            let env' = env |> VarMap.add var (n_var, false) in
            let m' = m |> NodeMap.add s_var t_var in
            m', env') !pre_vars (m, env)
      in env', m'

  end
