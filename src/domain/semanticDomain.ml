open AbstractDomain
open Syntax
open Util
open SensitiveDomain
open SenSemantics
open TracePartDomain
open Printer
open Config

(*
**********************************
** Abstract Data Flow Semantics **
**********************************
*)

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
    let op_R_eq_mod res l r op cons a = (*cons for flag of linear constraints*)
      match op with
      | Eq | Ne -> (match a with
        | Int v -> Int (AbstractValue.operator res l r op (-1) true v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator res l r op 1 true vt, AbstractValue.operator res l r (rev_op op) 0 true vf)
        | Unit _ -> raise (Invalid_argument "opR: Given a unit type"))
      | _ -> raise (Invalid_argument "op_R_eq_mod: Only does Eq/Ne operators.")
    let op_R res l r op cons a = (*cons for flag of linear constraints*)
      match op with
      | Plus | Mult | Div | Mod | Modc | Minus -> (match a with
        | Int v -> Int (AbstractValue.operator res l r op (-1) false v)
        | _ -> raise (Invalid_argument "opR: Given a unit type"))
      | Ge | Eq | Ne | Lt | Gt | Le -> (if cons then
        (match a with
        | Int v -> Int (AbstractValue.operator res l r op (-1) false v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator res l r op 1 false vt, AbstractValue.operator res l r (rev_op op) 0 false vf)
        | Unit _ -> raise (Invalid_argument "opR: Given a unit type")
        )
        else
        (match a with
        | Int v -> Bool (AbstractValue.operator res l r op 1 false v, AbstractValue.operator res l r (rev_op op) 0 false v)
        | Bool (vt, vf) -> Bool (AbstractValue.operator res l r op 1 false vt, AbstractValue.operator res l r (rev_op op) 0 false vf)
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
    let parallel_assign_R ress eabs = function
      | Int v -> Int (AbstractValue.parallel_assign ress eabs v)
      | _ -> raise (Invalid_argument "Parallel assign boolean does not support")
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
    let get_ae_from_R r = match r with
      | Int r1 -> Relation (forget_R "cur_v" r)
      | Bool (rt, rf) -> Relation (join_R (extrac_bool_R r true) (extrac_bool_R r false))
      | Unit _ -> Relation r

    (********************************************
     ** Abstract domain for Effects            **
     ********************************************)
    let union_eff f eff1 eff2 = 
      StateMap.union 
        (fun q acc1 acc2 -> Some (VarMap.union (fun v r1 r2 -> Some (f r1 r2)) acc1 acc2)) 
        eff1 eff2
    let merge_eff f eff1 eff2 = 
      StateMap.merge
        (fun q macc1 macc2 ->
          match macc1, macc2 with 
          | None, _ | _, None -> None
          | Some acc1, Some acc2 -> 
             Some (VarMap.merge 
                     (fun v mr1 mr2 ->
                       match mr1, mr2 with 
                       | None, _ | _, None -> None
                       | Some r1, Some r2 -> Some (f r1 r2)) 
                     acc1 acc2)) 
        eff1 eff2
    let fold_eff: (relation_e option -> relation_e option -> 'a) -> 
                  ('a -> bool) ->
      (relation_e VarMap.t StateMap.t) -> (relation_e VarMap.t StateMap.t) ->
      'a -> 'a = 
      fun f g eff1 eff2 res -> 
      StateMap.merge 
        (fun k macc1 macc2 -> match macc1, macc2 with None, None -> None | _, _ -> Some (macc1, macc2))
        eff1 eff2
      |> (fun ea -> StateMap.fold 
                   (fun q (macc1, macc2) res -> 
                     if g res
                     then res
                     else match macc1, macc2 with
                          | Some acc1, None -> VarMap.fold
                                                (fun v r1 res -> 
                                                  if g res 
                                                  then res
                                                  else f (Some r1) None) acc1 res
                          | None, Some acc2 -> VarMap.fold
                                                (fun v r2 res ->
                                                  if g res
                                                  then res
                                                  else f None (Some r2)) acc2 res
                          | Some acc1, Some acc2 -> 
                             VarMap.merge 
                               (fun v mr1 mr2 -> match mr1, mr2 with
                                              | None, None -> None
                                              | _, _ -> Some (mr1, mr2))
                               acc1 acc2
                             |> (fun mra -> VarMap.fold
                                           (fun v (mr1, mr2) res -> 
                                             if g res 
                                             then res
                                             else f mr1 mr2) mra res)
                          | None, None -> res
                   ) ea res)     
    let alpha_rename_Eff e prevar var = 
      effmapi (fun q eff -> VarMap.mapi (fun _ r -> alpha_rename_R r prevar var) eff) e
    let join_Eff e1 e2 = match e1, e2 with 
      | EffBot, _ | _, EffTop -> e2 
      | Effect eff1, Effect eff2 -> Effect (union_eff join_R eff1 eff2)
      | EffTop, _ | _, EffBot -> e1 
    let meet_Eff e1 e2 = match e1, e2 with 
      | EffBot, _ | _, EffTop -> e1
      | Effect e1, Effect e2 -> Effect (merge_eff meet_R e1 e2) 
      | EffTop, _ | _, EffBot -> e2 
    let leq_Eff e1 e2 = match e1, e2 with 
      | EffBot, _  | _, EffTop -> true
      | Effect e1, Effect e2 -> 
         fold_eff 
           (fun mr1 mr2 -> match mr1, mr2 with
                        | Some r1, Some r2 -> leq_R r1 r2
                        | None, Some _ -> true
                        | Some _, None -> false
                        | None, None -> true) 
           (not) e1 e2 true           
      | _,  _ -> false
    let eq_Eff e1 e2 = match e1, e2 with 
      | EffBot, EffBot | EffTop, EffTop -> true
      | Effect e1, Effect e2 -> 
         fold_eff 
           (fun mr1 mr2 -> match mr1, mr2 with
                        | Some r1, Some r2 -> eq_R r1 r2
                        | None, None -> true
                        | _, _ -> false)
           (not) e1 e2 true
      | _, _ -> false
    let forget_Eff var e = effmapi (fun q acc -> VarMap.mapi (fun v r -> forget_R var r) acc) e
    let arrow_Eff var e r = match e with 
      | EffBot -> EffBot 
      | Effect _ -> effmapi (fun q acc -> VarMap.mapi (fun v e_r -> arrow_R var e_r r) acc) e
      | EffTop -> raise (Invalid_argument "EffTop A should not be inferred")
    let equal_Eff e var = effmapi (fun q acc -> VarMap.mapi (fun v r -> equal_R r var) acc) e
    let wid_Eff e1 e2 = match e1, e2 with 
      | Effect e1, Effect e2 -> Effect (union_eff wid_R e1 e2)
      | _, _ -> join_Eff e1 e2
    let replace_Eff e var x = effmapi (fun q acc -> VarMap.mapi (fun v r -> replace_R r var x) acc) e
    let is_bot_acc acc = VarMap.fold (fun _ va res -> if res then is_bot_R va else res) acc true
    let minimize_eff = StateMap.filter (fun _ acc -> not @@ is_bot_acc acc)
    let is_bot_Eff e = match e with
      | EffBot -> true
      | Effect map -> minimize_eff map |> StateMap.is_empty
      | EffTop -> false
    let is_top_Eff e = match e with
      | EffBot -> true
      | Effect map -> false
      | EffTop -> false
    let stren_Eff e ae = match e, ae with
      | EffBot, _ -> EffBot 
      | Effect _, Relation rae -> 
         if is_bot_R rae 
         then EffBot 
         else (effmapi (fun q acc -> VarMap.mapi (fun v r -> stren_R r rae) acc) e
               |> effmap minimize_eff)
      | EffTop, Relation rae -> EffTop (* raise (Invalid_argument "EffTop B should not be inferred") *)
         (* effmapi (fun q r -> stren_R r) rae) eff_Top; where eff_Top = StateMap.creat (Q.size) (top_R Plus) *)
         (* 
         begin match rae with 
         | Int _ -> Effect rae
         | _ -> raise (Invalid_argument "ae should be {v: Int}")
         end*) 
      | _, Top -> e
      | _, Bot -> EffBot
      | _ -> raise (Invalid_argument "ae should not be a table") 
    let proj_Eff e vars = effmapi (fun q acc -> VarMap.mapi (fun v r -> proj_R r vars) acc) e
    let bot_R_Eff e = effmapi (fun q acc -> VarMap.mapi (fun v r -> bot_R Plus) acc) e
    let bot_Eff = EffBot 
    let empty_eff = Effect (StateMap.empty)  (* todo: must revisit. it should be  a map where all states map to Bot *)
          
    let extract_v = function
      | TEBot -> Bot
      | TypeAndEff (v, _) -> v
      | TETop -> Top
    let extract_eff = function
      | TEBot -> EffBot
      | TypeAndEff (_, e) -> e
      | TETop -> EffTop

    let extract_ec = 
      if (!Config.effect_on && (not !Config.ev_trans)) then
          (fun te -> 
            match (extract_eff te) with
            | EffBot | EffTop -> 
              (if !debug then
                let pr = Format.fprintf Format.std_formatter in
                pr ("@.ERR(extract_ec)-> node: %a \
                      An effect should be observable at this stage of the analysis")
                  pr_value_and_eff te);
              StateMap.empty
            | Effect e -> e)
          else
            (fun te -> StateMap.empty)
    (* let get_ae_from_eff = function
      | EffBot -> Bot
      | EffTop -> Top
      | Effect e -> StateMap.fold (fun q acc_t acc -> join_V acc acc) e Bot *)

    (*
     ********************************
     ** Abstract domains for       **
     **     Values                 **
     **     and ValueAndEffects    **
     ********************************
     *)
    let init_VE_v v = TypeAndEff (v, EffBot)
    let destruct_VE = function 
      | TEBot -> Bot, EffBot
      | TypeAndEff (v, e) -> v, e
      | TETop -> Top, EffTop 
    let rec alpha_rename_V v prevar var = match v with
      | Relation r -> Relation (alpha_rename_R r prevar var)
      | Table t -> Table (alpha_rename_T alpha_rename_VE t prevar var)
      | Ary ary -> Ary (alpha_rename_Ary ary prevar var)
      | Lst lst -> Lst (alpha_rename_Lst lst prevar var)
      | Tuple u -> Tuple (alpha_rename_Tuple u prevar var)
      | _ -> v
    and alpha_rename_VE ve prevar var = match ve with 
      | TypeAndEff (v, e) -> TypeAndEff (alpha_rename_V v prevar var, alpha_rename_Eff e prevar var)
      | _ -> ve
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
    and join_V (v1:value_tt) (v2:value_tt) :value_tt = match v1, v2 with
      | Bot, v | v, Bot -> v
      | Relation r1, Relation r2 -> Relation (join_R r1 r2)
      | Table t1, Table t2 -> Table (join_T join_VE alpha_rename_VE t1 t2)
      | Ary ary1, Ary ary2 -> Ary (join_Ary ary1 ary2)
      | Lst lst1, Lst lst2 -> Lst (join_Lst lst1 lst2)
      | Tuple u1, Tuple u2 -> Tuple (join_Tuple u1 u2)
      | _, _ -> Top
    and join_VE (ve1: value_te) (ve2: value_te) :value_te = match ve1, ve2 with 
      | TEBot, ve | ve, TEBot -> ve
      | TypeAndEff (v1, e1), TypeAndEff (v2, e2) -> TypeAndEff (join_V v1 v2, join_Eff e1 e2)
      | _, _ -> TETop
    and meet_V (v1:value_tt) (v2:value_tt) :value_tt = match v1, v2 with
      | Top, v | v, Top -> v
      | Relation r1, Relation r2 -> Relation (meet_R r1 r2)
      | Table t1, Table t2 -> Table (meet_T meet_VE alpha_rename_VE t1 t2)
      | Ary ary1, Ary ary2 -> Ary (meet_Ary ary1 ary2)
      | Lst lst1, Lst lst2 -> Lst (meet_Lst lst1 lst2)
      | Tuple u1, Tuple u2 -> Tuple (meet_Tuple u1 u2)
      | _, _ -> Bot
    and meet_VE (ve1: value_te) (ve2: value_te) :value_te = match ve1, ve2 with 
      | TETop, ve | ve, TETop -> ve
      | TypeAndEff (v1, e1), TypeAndEff (v2, e2) -> TypeAndEff (meet_V v1 v2, meet_Eff e1 e2)
      | _, _ -> TEBot
    and leq_V (v1:value_tt) (v2:value_tt) :bool = match v1, v2 with
      | Bot, _ -> true
      | _, Top -> true
      | Relation r1, Relation r2 -> leq_R r1 r2
      | Table t1, Table t2 -> leq_T leq_VE t1 t2
      | Ary ary1, Ary ary2 -> leq_Ary ary1 ary2
      | Lst lst1, Lst lst2 -> leq_Lst lst1 lst2
      | Tuple u1, Tuple u2 -> leq_Tuple u1 u2
      | _, _ -> false
    and leq_VE (ve1: value_te) (ve2: value_te) :bool = match ve1, ve2 with 
      | TEBot, _ -> true
      | _, TETop -> true
      | TypeAndEff (v1, e1), TypeAndEff (v2, e2) -> (leq_V v1 v2) && (leq_Eff e1 e2) 
      | _, _ -> false
    and sat_leq_V (v1:value_tt) (v2:value_tt) :bool = match v1, v2 with
      | Bot, _ -> true
      | _, Top -> true
      | Relation r1, Relation r2 -> leq_R r1 r2
      | Table t1, Table t2 -> leq_T leq_VE t1 t2
      | Ary ary1, Ary ary2 -> leq_Ary ary1 ary2
      | Lst lst1, Lst lst2 -> sat_leq_Lst lst1 lst2
      | Tuple u1, Tuple u2 -> leq_Tuple u1 u2
      | _, _ -> false
    and sat_leq_VE (ve1: value_te) (ve2: value_te) :bool = match ve1, ve2 with 
      | TEBot, _ -> true
      | _, TETop -> true
      | TypeAndEff (v1, e1), TypeAndEff (v2, e2) -> (sat_leq_V v1 v2) && (leq_Eff e1 e2) 
      | _, _ -> false
    and eq_V (v1:value_tt) (v2:value_tt) :bool = match v1, v2 with
      | Bot, Bot -> true
      | Top, Top -> true
      | Relation r1, Relation r2 -> eq_R r1 r2
      | Table t1, Table t2 -> eq_T eq_VE t1 t2
      | Ary ary1, Ary ary2 -> eq_Ary ary1 ary2
      | Lst lst1, Lst lst2 -> eq_Lst lst1 lst2
      | Tuple u1, Tuple u2 -> eq_Tuple u1 u2
      | _, _ -> false
    and eq_VE (ve1: value_te) (ve2: value_te) :bool = match ve1, ve2 with 
      | TEBot, TEBot -> true
      | TETop, TETop -> true
      | TypeAndEff (v1, e1), TypeAndEff (v2, e2) -> (eq_V v1 v2) && (eq_Eff e1 e2) 
      | _, _ -> false
    and is_table (v:value_tt) = match v with
      | Table _ -> true
      | _ -> false
    and is_bool_V (v:value_tt) = match v with
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
    (*todo: if v' is Bot then the result should be Bot? why is it different from the paper *)
    and arrow_V var (v:value_tt) (v':value_tt) = match v' with
      | Bot | Top | Table _ -> v
      | Relation r2 -> (match v with
                       | Table t -> Table (arrow_T forget_V arrow_VE var t v')
                       | Relation r1 -> Relation (arrow_R var r1 r2)
                       | Ary ary -> Ary (arrow_Ary var ary r2 None)
                       | Lst lst -> Lst (arrow_Lst var lst (Relation r2) None)
                       | Tuple u -> Tuple (arrow_Tuple var u v')
                       | _ -> v)
      | Tuple u2 -> List.fold_left (fun v1 (ve2, i) ->
                       let var_i = var^"."^(string_of_int i) in
                       match ve2 with 
                       | TETop | TEBot -> v1
                       | TypeAndEff (v2, _) -> arrow_V var_i v1 v2) v (List.length u2 |> first_n |> zip_list u2)
      | Ary ary2 -> let (vars, (rl2, re2)) = ary2 in
                   (match v with
                    | Table t -> Table (arrow_T forget_V arrow_VE var t v')
                    | Relation r1 -> Relation (
                                        let r1' = arrow_R var r1 rl2 in
                                        if is_bot_R re2 then r1' else
                                          arrow_R var r1' re2
                                      )
                    | Ary ary -> Ary (arrow_Ary var ary re2 (Some rl2))
                    | Lst lst -> Lst (arrow_Lst var lst (Relation re2) (Some (vars, rl2)))
                    | _ -> v)
      | Lst lst2 -> let ((l2,e2) as vars, (rl2, vee2)) = lst2 in
                   (match v with
                    | Table t -> Table (arrow_T forget_V arrow_VE var t v')
                    | Relation r1 -> if is_bot_R rl2 then Relation (bot_shape_R r1) else
                                      let r1' = let res = meet_R r1 rl2 in
                                                if is_bot_R res then (meet_R (forget_R l2 r1) rl2) else res in
                                      (match vee2 with 
                                       | TEBot -> if String.length var >= 2 && String.sub var 0 2 = "zh" then
                                                 raise (Invalid_argument "List.hd expects non empty list")
                                                 else Relation r1'
                                       | TypeAndEff (Relation re2, _) ->
                                          if is_bot_R re2 then Relation r1'
                                          else Relation (meet_R r1' (proj_R re2 [e2]))
                                       | TypeAndEff (Tuple ue, _) -> Relation r1'
                                       | TypeAndEff (ve2, _) -> arrow_V var (Relation r1') ve2
                                       | TETop -> Relation r1')
                    | Ary ary -> (match vee2 with
                                 | TypeAndEff (Relation re2, _) -> Ary (arrow_Ary var ary re2 (Some rl2))
                                 | _ -> Ary (arrow_Ary var ary rl2 None))
                    | Lst lst -> (match vee2 with 
                                 | TypeAndEff (ve2, _) -> Lst (arrow_Lst var lst ve2 (Some (vars, rl2)))
                                 | _ -> Lst lst)
                    | Top -> if String.sub var 0 2 = "zh" then
                              (match vee2 with
                               | TypeAndEff (Relation re2, _) -> 
                                  let r1' = arrow_R var (equal_R (top_R Plus) e2) rl2 in
                                  arrow_V var (Relation r1') (Relation re2)
                               | TypeAndEff (ve2, _) -> ve2 
                               | TETop -> Top 
                               | TEBot -> Bot)
                            else v
                    | _ -> v)
    (* todo: same here, the strengthening with Bot return the effect. Need to verify why is that? *)
    and arrow_EffV var e v' = 
      let arrow_Eff_with_lst eff l =
          let ((l2,e2) as vars, (rl2, vee2)) = l in 
          let arrow_EffR er = 
            if is_bot_R rl2 then bot_shape_R er 
            else
              let er' = let res = meet_R er rl2 in
                        if is_bot_R res then (meet_R (forget_R l2 er) rl2) else res in
              (match vee2 with 
               | TEBot -> if String.length var >= 2 && String.sub var 0 2 = "zh" then
                           raise (Invalid_argument "List.hd expects non empty list")
                         else er'
               | TypeAndEff (Relation re2, _) ->
                  if is_bot_R re2 then er'
                  else meet_R er' (proj_R re2 [e2])
               | TypeAndEff (Tuple ue, _) -> er'
               | TypeAndEff (ve2, _) -> (match arrow_V var (Relation er') ve2 with
                                        | Relation er'' -> er''
                                        | _ -> raise (Invalid_argument ("Arrow operator applied to a " ^
                                                       "Relation should be relation")))
               | TETop -> er')
          in
          effmapi (fun q acc -> VarMap.mapi (fun v er -> arrow_EffR er) acc) eff
      in
      match e, v' with 
      | EffBot,  _ -> EffBot
      | EffTop, _ -> EffTop
      | _, Bot | _, Top | _, Table _ -> e
      | _, Relation r2 -> arrow_Eff var e r2
      | _, Tuple u2 -> List.fold_left (fun e1 (ve2, i) ->
          let var_i = var^"."^(string_of_int i) in
          match ve2 with 
          | TETop | TEBot -> e1
          | TypeAndEff (v2, _) -> arrow_EffV var_i e1 v2) e (List.length u2 |> first_n |> zip_list u2)
      | _, Ary ary2 -> let (vars, (rl2, re2)) = ary2 in
                   let e' = arrow_Eff var e rl2 in 
                   if is_bot_R re2 then e' else arrow_Eff var e' re2
      | ((Effect _) as eff), Lst lst2 -> arrow_Eff_with_lst eff lst2                  
    and arrow_VE var ve v' = match ve with 
      | TypeAndEff (v, e) -> TypeAndEff (arrow_V var v v', arrow_EffV var e v')
      | _ -> ve
    and forget_V var v = match v with
      | Table t -> Table (forget_T forget_VE var t)
      | Ary ary -> Ary (forget_Ary var ary)
      | Lst lst -> Lst (forget_Lst var lst)
      | Relation r -> Relation (forget_R var r)
      | Tuple u -> Tuple (forget_Tuple var u)
      | _ -> v
    and forget_VE var ve = match ve with
      | TypeAndEff (v, e) -> TypeAndEff (forget_V var v, forget_Eff var e)
      | _ -> ve
    and equal_V v var = match v with
      | Relation r -> Relation (equal_R r var)
      | Table t -> Table (equal_T equal_VE alpha_rename_VE t var)
      | Tuple u -> Tuple (equal_Tuple u var)
      | _ -> v
    and equal_VE ve var = 
      temap ((fun v -> equal_V v var), (fun e -> equal_Eff e var)) ve
    and wid_V v1 v2 = match v1, v2 with
      | Relation r1, Relation r2 -> Relation (wid_R r1 r2)
      | Table t1, Table t2 -> Table (wid_T wid_VE alpha_rename_VE t1 t2)
      | Ary ary1, Ary ary2 -> Ary (wid_Ary ary1 ary2)
      | Lst lst1, Lst lst2 -> Lst (wid_Lst lst1 lst2)
      | Tuple u1, Tuple u2 -> Tuple (wid_Tuple u1 u2)
      | _, _ -> join_V v1 v2
    and wid_VE ve1 ve2 = match ve1, ve2 with 
      | TypeAndEff (v1, e1), TypeAndEff (v2, e2) -> TypeAndEff ((wid_V v1 v2), (wid_Eff e1 e2))
      | _, _ -> join_VE ve1 ve2
    and op_V sl sr op mod_eq_flag v = match v with
      | Bot | Top -> Top
      | Relation r -> Relation (if mod_eq_flag then op_R_eq_mod "" sl sr op false r else op_R "" sl sr op false r)
      | _ -> raise (Invalid_argument "Should be a relation type when using op_V")
    and op_VE sl sr op mod_eq_flag ve = temap ((op_V sl sr op mod_eq_flag), id) ve
    and uop_V op s v = match v with
      | Bot | Top -> Top
      | Relation r -> Relation (uop_R "" op s false r)
      | _ -> raise (Invalid_argument "Should be a relation type when using uop_V")
    and uop_VE op s ve = temap ((uop_V op s), id) ve
    and bool_op_V op v1 v2 = match v1, v2 with
      | Bot, Relation _ -> if string_of_op op = "&&" then Bot else v2
      | Top, Relation _ -> if string_of_op op = "&&" then v2 else Top
      | Relation r1, Relation r2 -> Relation (bool_op_R op r1 r2)
      | Relation _, Bot -> if string_of_op op = "&&" then Bot else v1
      | Relation _, Top -> if string_of_op op = "&&" then v1 else Top
      | _, _ -> raise (Invalid_argument "Should be a relation type when using bool_op_V")
    and bool_op_VE op ve1 ve2 = match ve1, ve2 with
      | TEBot, TypeAndEff (v,e) -> TypeAndEff ((bool_op_V op Bot v), e)
      | TETop, TypeAndEff (v,e) -> TypeAndEff ((bool_op_V op Top v), e)
      | (TypeAndEff (v1, e1)), (TypeAndEff (v2, e2)) -> TypeAndEff ((bool_op_V op v1 v2), e2)
      | _, _ -> raise (Invalid_argument "Should be a relation type when using bool_op_VE")
    and is_Bot_V = function
      | Bot -> true
      | _ -> false
    and replace_V v var x = match v with
      | Table t -> Table (replace_T replace_VE t var x)
      | Relation r -> Relation (replace_R r var x)
      | Ary ary -> Ary (replace_Ary ary var x)
      | Lst lst -> Lst (replace_Lst lst var x)
      | Tuple u -> Tuple (replace_Tuple u var x)
      | _ -> v
    and replace_VE ve var x = 
      temap ((fun v -> replace_V v var x), (fun e -> replace_Eff e var x)) ve
    and extrac_bool_V v b = match v with
      | Relation r -> Relation (extrac_bool_R r b)
      | _ -> raise (Invalid_argument "Should be relation when split if statement")
    and stren_V v ae = match v,ae with
      | Bot, _ -> Bot
      | Relation r, Relation rae -> if is_bot_R rae && is_unit_R r then Bot else
                                     Relation (stren_R r rae)
      | Ary ary, Relation rae -> Ary (stren_Ary ary rae)
      | Lst lst, Relation rae -> Lst (stren_Lst lst rae)
      | Table t, Relation rae -> Table t (* Table (stren_T stren_V t ae) *)
      | Tuple u, Relation rae -> Tuple (stren_Tuple u ae)
      | Top, _ -> ae
      | _, Bot -> Bot
      | _, Top -> v
      | _,_ -> raise (Invalid_argument "ae should not be a table")
    and stren_VE ve ae = 
      let ve' = temap ((fun v -> stren_V v ae), (fun e -> stren_Eff e ae)) ve in 
      match ve' with 
      | TETop -> failwith "todo: not implemented. need further insights"
      | _ -> ve'
    and stren_ite_V v ae = match v, ae with
      | Table t, Relation rae -> 
         if is_bot_R rae then Table t else Table (stren_T stren_VE t ae)
      | _, _ -> stren_V v ae
    and stren_ite_VE ve ae = 
      let ve' = temap ((fun v ->  stren_ite_V v ae), (fun e -> stren_Eff e ae)) ve in 
      match ve' with 
      | TETop -> failwith "todo: not implemented. need further insights"
      | _ -> ve'
    and sat_equal_V v x = match v with
      | Relation r -> sat_equal_R r x
      | Tuple u -> sat_equal_Tuple u x
      | _ -> false
    and sat_equal_VE ve x = match ve with
      | TypeAndEff (v, _) -> sat_equal_V v x
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
      | Table t -> Table (proj_T proj_VE get_arg_vars_VE t vars)
      | Ary ary -> Ary (proj_Ary ary vars)
      | Lst lst -> Lst (proj_Lst lst vars)
      | Tuple u -> Tuple (proj_Tuple u vars)
      | _ -> v
    and proj_VE ve vars = 
      temap ((fun v -> proj_V v vars), (fun e -> proj_Eff e vars)) ve
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
    (* and get_second_table_input_V = function
      | Bot -> Bot
      | Table t -> if table_isempty t then Bot else
                    let _,(_,veo) = get_full_table_T t in
                    (match veo with
                     | TEBot -> Bot
                     | TypeAndEff (Table t, _) -> if table_isempty t then Bot else
                                                   (let _,(vei,_) = get_full_table_T t in 
                                                   match vei with
                                                   | TEBot -> Bot
                                                   | TypeAndEff (vi, _) -> vi 
                                                   | TETop -> raise (Invalid_argument "input should not be Top"))
                     | _ -> raise (Invalid_argument "array.set should be a table"))
      | _ -> raise (Invalid_argument "array.set should be a table") *)
    and join_for_item_V v1 v2 = match v1, v2 with
      | Bot, _ | _, Bot -> v1
      | Ary ary, Relation r -> Ary (join_for_item_Ary ary r)
      | _,_ -> raise (Invalid_argument "Approximate array's item should use relation type")
    and extrac_item_V vars = function
      | Bot | Top as v -> v
      | Lst lst -> extrac_item_Lst vars lst
      | _ -> raise (Invalid_argument "item inside either a list or an array")
    and extrac_item_VE vars = function
      | TEBot | TETop as ve -> ve
      | TypeAndEff ((Lst lst), e) -> TypeAndEff ((extrac_item_Lst vars lst), e)
      | _ -> raise (Invalid_argument "item inside either a list or an array")
    and reduce_len_V len le_lst = function
      | Bot | Top as v -> v
      | Lst lst -> Lst (reduce_len_Lst len le_lst lst)
      | _ -> raise (Invalid_argument "reduce length either a list or an array")
    and reduce_len_VE len le_lst = function
      | TEBot | TETop as v -> v
      | TypeAndEff ((Lst lst), e) -> TypeAndEff ((Lst (reduce_len_Lst len le_lst lst)), e)
      | _ -> raise (Invalid_argument "reduce length either a list or an array")
    and list_cons_V v1 v2 = match v1, v2 with
      | Bot, _ | _, Bot -> Bot
      | v, Lst lst -> Lst (list_cons_Lst v lst)
      | _,_ -> raise (Invalid_argument "List construct should be item :: lst")
    and list_cons_VE ve1 ve2 =  match ve1, ve2 with 
      | TEBot, _ | _, TEBot -> TEBot
      | (TypeAndEff (v, _)), (TypeAndEff ((Lst lst), e)) -> TypeAndEff ((Lst (list_cons_Lst v lst)), e)
      | _ -> raise (Invalid_argument "reduce length either a list or an array")
    and alpha_rename_Vs v1 v2 = match v1, v2 with
      | Lst (((l1,e1), (rl1,vee1)) as lst1), Lst (((l2,e2), (rl2,vee2)) as lst2) ->
         if l1 = l2 && e1 = e2 then Lst lst1, Lst lst2 else
           let lst2 = 
             ((l2,e2), (rl2 |> forget_R l1 |> forget_R e1,
                        vee2 |> forget_VE l1 |> forget_VE e1)) in
           let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
           Lst lst1', Lst lst2'
      | Ary ary1, Ary ary2 -> let ary1', ary2' =  alpha_rename_Arys ary1 ary2 in
                             Ary ary1', Ary ary2'
      | _, _ -> v1, v2
    and alpha_rename_VEs ve1 ve2 = match ve1, ve2 with
      | (TypeAndEff (v1, e1)), (TypeAndEff (v2, e2)) -> 
         let v1', v2' = alpha_rename_Vs v1 v2 in
         (TypeAndEff (v1', e1)), (TypeAndEff (v2', e2))
      | _, _ -> ve1, ve2
    and bot_relation_V (tp: inputType) = match tp with
      | Int -> Relation (bot_R Plus)
      | Bool -> Relation (bot_R Ge)
      | Unit -> Tuple []
    and get_arg_vars_VE var = function
      | TypeAndEff (Lst lst, _) -> get_list_length_item_Lst lst
      | TypeAndEff (Tuple u, _) -> get_vars_Tuple var u
      | _ -> []
    and get_list_length_item_VE = function
      | TypeAndEff (Lst lst, _) -> get_list_length_item_Lst lst
      | _ -> []
    and only_shape_V = function
      | Relation r -> is_bot_R r
      | Lst lst -> only_shape_Lst lst
      | Ary ary -> only_shape_Ary ary
      | Tuple u -> only_shape_Tuple u
      | Bot -> true
      | _ -> false
    and only_shape_VE = function
      | TEBot -> true
      | TypeAndEff (t, e) -> only_shape_V t && is_bot_Eff e
      | _ -> false
    and is_bot_VE = function
      | TEBot -> true
      | TypeAndEff (Bot, e) -> is_bot_Eff e
      | _ -> false
    and is_top_VE = function
      | TETop -> true
      | TypeAndEff (Top, e) -> is_top_Eff e
      | _ -> false
    and cons_temp_lst_V t = function
      | Lst lst -> Lst (cons_temp_lst_Lst t lst)
      | _ -> raise (Invalid_argument "Temp list construct should be item :: lst")
    and cons_temp_lst_VE t = function
      | TypeAndEff ((Lst lst), e) -> TypeAndEff ((Lst (cons_temp_lst_Lst t lst)), e)
      | _ -> raise (Invalid_argument "Temp list construct should be item :: lst")
    and item_shape_V te t = match te, t with
      | Lst lste, Lst lst ->
         Lst (item_shape_Lst lste lst)
      | _, _ -> t
    and item_shape_VE tee te = match tee, te with 
      | (TypeAndEff (v1, e1)), (TypeAndEff (v2, e2)) -> 
         TypeAndEff ((item_shape_V v1 v2), e2)
      | _, _ -> te
    and bot_shape_V = function
      | Bot | Top -> Bot
      | Relation r -> Relation (bot_shape_R r)
      | Table t -> Table (bot_shape_T bot_shape_VE t)
      | Lst lst -> Lst (bot_shape_Lst lst)
      | Ary ary -> Ary (bot_shape_Ary ary)
      | Tuple u -> Tuple (bot_shape_Tuple u)
    and bot_shape_VE ve =
      let ve' = temap (bot_shape_V, (fun _ -> bot_Eff)) ve in 
      match ve' with 
      | TEBot | TETop -> TEBot
      | _ -> ve'
    and add_tuple_item_V v v' = match v with
      | Tuple u -> Tuple (add_tuple_item_Tuple u v')
      | _ -> raise (Invalid_argument "Tuple constrct should be value, tuple")
    and is_tuple_V = function
      | Tuple u -> true
      | _ -> false
    and is_tuple_VE = function
      | TypeAndEff ((Tuple u), _) -> true
      | _ -> false
    and is_pure_Tuple ve =
      is_tuple_VE ve &&
      List.fold_right (fun ve is_pure -> is_pure && (is_Relation (extract_v ve) || is_pure_Tuple ve)) (get_tuple_list_VE ve) true
    and get_tuple_list_V = function
      | Tuple u -> get_tuple_list u
      | _ -> raise (Invalid_argument "extract tuple should be a tuple")
    and get_tuple_list_VE = function
      | TypeAndEff ((Tuple u), _) -> get_tuple_list u
      | _ -> raise (Invalid_argument "extract tuple should be a tuple")
    and pattern_empty_lst_V = function
      | Lst lst -> Lst (pattern_empty_Lst lst)
      | _ -> raise (Invalid_argument "pattern x::[] should give a list")
    and pattern_empty_lst_VE = function
      | TypeAndEff ((Lst lst), e)  -> TypeAndEff ((Lst (pattern_empty_Lst lst)), e)
      | _ -> raise (Invalid_argument "pattern x::[] should give a list")
    and rename_lambda_V v = match v with
      | Lst lst -> Lst (rename_lambda_Lst lst)
      | _ -> v 
    and get_ae_from_v v = match v with
      | Relation r -> get_ae_from_R r
      | Tuple u -> get_ae_from_Tuple u
      | _ -> raise (Invalid_argument "Meet: Base Type not equal")

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
      (varl, vare), (r1, TEBot)
    and const_Lst lst = 
      let varl, vare = fresh_length (), fresh_item () in
      let min, max = List.fold_left (fun (min, max) item -> 
        let min' = if min > item then item else min in
        let max' = if max < item then item else max in
        min', max') (max_int, min_int) lst in
      let rl = top_R Plus |> op_R varl varl (string_of_int (List.length lst)) Eq true in
      let re = rl |> op_R vare vare (string_of_int min) Ge true 
        |> op_R vare vare (string_of_int max) Le true in
      (varl, vare), (rl, TypeAndEff (Relation re, bot_Eff))
    and init_Lst vars : list_t = let varl, vare = vars in
      let varl' = if varl = "l" || varl = "l'" then fresh_length () else varl in
      let vare' = if vare = "e" || vare = "e'" then fresh_item () else vare in
      let vars = varl', vare' in
      vars, (bot_R Plus, TEBot)
    and get_len_var_Lst ((varl,_),_) = varl
    and get_item_var_Lst ((_,vare),_) = vare
    and pattern_empty_Lst ((l,e) as vars, (_, vee)) = 
      let rl' = top_R Plus |> op_R l l "0" Eq true in
      let vee' = bot_shape_VE vee in
      vars, (rl', vee')
    and extrac_item_Lst vars ((_,vare), (_, vee)) =
      (* let vars' = vare :: vars in *)
      let vee' = match vee with
      | TypeAndEff (Relation _, _) -> alpha_rename_VE vee vare "cur_v" |> extract_v
      | _ -> extract_v vee
        (* alpha_rename_R (proj_R re vars' |> forget_R "cur_v") vare "cur_v"  *)
      in
      vee'
    and join_Lst lst1 lst2 = 
      let (l1, e1), (rl1,vee1) = lst1 in
      let (l2, e2), (rl2,vee2) = lst2 in
      let lst1', lst2' = if l1 <> "l" && contains_var_R l1 rl2 then
        let a, b = alpha_rename_Lsts lst2 lst1 in 
        b, a
        else alpha_rename_Lsts lst1 lst2 in
      let (l1, e1) as vars1, (rl1,vee1) = lst1' in let (l2, e2) as vars2, (rl2,vee2) = lst2' in
      vars1, (join_R rl1 rl2, join_VE vee1 vee2)
    and meet_Lst lst1 lst2 = 
      let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
      let vars1, (rl1,vee1) = lst1' in let vars2, (rl2,vee2) = lst2' in
      vars1, (meet_R rl1 rl2, meet_VE vee1 vee2)
    and leq_Lst lst1 lst2 = 
      let (l1,e1), (rl1,vee1) = lst1 in let (l2,e2), (rl2,vee2) = lst2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then leq_R rl1 rl2 && leq_VE vee1 vee2 else false
    and sat_leq_Lst lst1 lst2 = 
      let (l1,e1), (rl1,vee1) = lst1 in let (l2,e2), (rl2,vee2) = lst2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then 
        let rl1 = proj_R rl1 [l1] in
        let rl2 = proj_R rl2 [l2] in
        leq_R rl1 rl2 && leq_VE vee1 vee2
      else false
    and eq_Lst lst1 lst2 =
      let (l1,e1), (rl1,vee1) = lst1 in let (l2,e2), (rl2,vee2) = lst2 in
      let scop_check = l1 = l2 && e1 = e2 in
      if scop_check then eq_R rl1 rl2 && eq_VE vee1 vee2 else false
    and wid_Lst lst1 lst2 =
      let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
      let vars1, (rl1,vee1) = lst1' in let vars2, (rl2,vee2) = lst2' in
      vars1, (wid_R rl1 rl2,wid_VE vee1 vee2)
    and arrow_Lst var lst v ropt = 
      let ((l,e) as vars, (rl,vee)) = lst in
      let eff' = match extract_eff vee with 
        | (Effect _) as e  -> e
        | _ -> EffBot
      in
      match ropt with
      | Some ((_, e') , rl') -> (match v, extract_v vee with
        | Bot, _ -> if String.sub var 0 2 = "xs" && contains_var_R "zc" rl' then
           let rl = arrow_R var rl rl' in
           let re' = (op_R "" e "zc" Eq true rl) in
           (vars, (rl, TypeAndEff ((Relation re'), eff')))
          else let rl = arrow_R var rl rl' in
           (vars, (rl, vee))
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
            (vars, (rl, TypeAndEff ((Relation re'), eff')))
          else if String.sub var 0 2 = "zt" then
            let rl = arrow_R var rl rl' in
            let re' = arrow_R var re r |> (op_R "" e e' Eq true) in
            (vars, (rl, TypeAndEff ((Relation re'), eff')))
          else 
            let rl' = forget_R l rl' |> forget_R e in
            let r' = forget_R l r |> forget_R e in
          (vars, (arrow_R var rl rl', TypeAndEff ((Relation (arrow_R var re r')), eff')))
        | Relation r, _ -> 
          if String.sub var 0 2 = "xs" then
           let rl = arrow_R var rl rl' in
           let re' = 
            let r' = (op_R "" e e' Eq true rl) in
            let r'' = (op_R "" e "zc" Eq true rl) in
            join_R r' r''
           in
           (vars, (rl, TypeAndEff ((Relation re'), eff')))
          else 
            let rl = arrow_R var rl rl' in
            let vee' = arrow_VE var vee v in
            (vars, (rl, vee'))
        | _, _ -> 
          if String.sub var 0 2 = "xs" then
            let rl = arrow_R var rl rl' in
            let vee' = 
              if is_Bot_V v then 
              (* TODO: how could we get abstract value without change transformer *)
                vee 
              else 
                let v' = arrow_VE var vee v in
                let v'' = arrow_VE var vee v in
                join_VE v' v''
            in
            (vars, (rl,  vee'))
          else if String.sub var 0 2 = "zt" then
            let rl = arrow_R var rl rl' in
            let vee' = TypeAndEff (v, eff') in
            (vars, (rl, vee'))
          else 
            let rl' = forget_R l rl' |> forget_R e in
            let v' = forget_V l v |> forget_V e in
          (vars, (arrow_R var rl rl', arrow_VE var vee v'))
      )
      | None -> 
        match v, extract_v vee with
        | Relation r, Relation re ->
        let r' = forget_R l r |> forget_R e in
        (vars, (arrow_R var rl r', TypeAndEff ((Relation (arrow_R var re r')), eff')))
        | Relation r, _ -> let r' = forget_R l r in
          (vars, (arrow_R var rl r',arrow_VE var vee v))
        | _, _ -> vars, (rl,arrow_VE var vee v)
    and forget_Lst var lst = let (vars, (rl,vee)) = lst in
      (vars, (forget_R var rl, forget_VE var vee))
    and stren_Lst lst ae = let ((l,e) as vars, (rl,vee)) = lst in
      let ae' = (forget_R e ae |> forget_R l) in
      (vars, (stren_R rl ae', stren_VE vee (Relation ae')))
    and proj_Lst lst vars = let ((l,e), (rl,vee)) = lst in
      let vars' = e :: l :: vars in
      ((l,e), (proj_R rl vars', proj_VE vee vars'))
    and alpha_rename_Lsts lst1 lst2 = 
      let (l1,e1), _ = lst1 in let (l2,e2), _ = lst2 in
      let lst2' = match l1 = l2, e1 = e2 with
        | true, true -> lst2
        | false, true -> alpha_rename_Lst lst2 l2 l1
        | true, false -> alpha_rename_Lst lst2 e2 e1
        | false, false -> let lst2 = alpha_rename_Lst lst2 l2 l1 in alpha_rename_Lst lst2 e2 e1
      in
      lst1, lst2'
    and alpha_rename_Lst lst prevar var = let (l,e), (rl,vee) = lst in
      let l' = if l = prevar then var else l in
      let e' = if e = prevar then var else e in
      let rl' = alpha_rename_R rl prevar var in
      let vee' = alpha_rename_VE vee prevar var in
      (l',e'), (rl',vee')
    and rename_lambda_Lst lst = let (l,e), (rl,vee) = lst in
      let varl, vare = fresh_length (), fresh_item () in
      let rl' = alpha_rename_R rl l varl in
      let vee' = alpha_rename_VE vee e vare in
      (varl, vare), (rl', vee')
    and replace_Lst lst var x = let ((l,e), (rl,vee)) = lst in
      let l' = if l = var then x else l in
      let e' = if e = var then x else e in
      let rl' = replace_R rl var x in
      let vee' = replace_VE vee var x in
      (l',e'), (rl',vee')
    and reduce_len_Lst len le_lst lst = let ((l,e), (rl,vee)) = lst in
      let l', e' = 
        match le_lst with
        | [] -> fresh_length (), fresh_item()
        | l' :: e' :: [] -> if l = l' then fresh_length (), fresh_item() else l', e'
        | _ -> raise (Invalid_argument "construct pattern tl should give [] or [l;e]")
       in
      let rl' = assign_R l' l (string_of_int len) Minus rl |> op_R "" l' "0" Ge true
        (* |> op_R l' l "1" Minus true  *)
      in
      let vee' = 
        let eff' = match extract_eff vee with 
          | (Effect _) as e  -> e
          | _ -> EffBot
        in
        match extract_v vee with
        | Relation re -> 
          let r' = (meet_R (forget_R l re) rl') in
          TypeAndEff ((Relation (alpha_rename_R r' e e')), eff')
        | _ -> vee
      in
      (l',e'), (rl',vee')
    and list_cons_Lst v lst = let ((l,e), (rl,vee)) = lst in
      if v = Bot || is_bot_R rl then (l,e), (bot_R Plus, TEBot) else
      (* let v = stren_V v (Relation (forget_R l rl)) in *)
      let rl' = assign_R l l "1" Plus rl in
      let eff' = match extract_eff vee with 
        | (Effect _) as e  -> e
        | _ -> EffBot
      in
      let vee' = match v, extract_v vee with
        | Relation r, Bot ->
          TypeAndEff ((Relation (arrow_R e (rl') r)), eff')
        | Relation r, Relation re -> 
          let re' = arrow_R e rl' (forget_R l r) |> join_R (meet_R (forget_R l re) rl') in
          TypeAndEff ((Relation re'), eff')
        | Table t, Bot -> let t' = init_T (dx_T (TypeAndEff (v, empty_eff))) in
          TypeAndEff ((Table t'), eff')
        | Tuple u, Bot -> let u' = List.init (List.length u) (fun _ ->
             TEBot) in TypeAndEff ((Tuple u'), eff')
        | _, ve -> TypeAndEff ((join_V v ve), eff')
      in
      (l,e), (rl',vee')
    and get_list_length_item_Lst ((l,e), _) = [l;e]
    and only_shape_Lst ((l,e), (rl,vee)) = 
      is_bot_R rl && vee = TEBot
    and prop_Lst prop ((l1,e1) as vars1, (rl1,vee1)) ((l2,e2) as vars2, (rl2,vee2)) =
      let rl1', rl2' = rl1, join_R rl1 rl2 in
      let vee1', vee2' = match vee1, vee2 with
      | _, TEBot | TEBot, _ -> vee1, vee1
      | TypeAndEff ((Relation re1), eff1), TypeAndEff ((Relation re2), eff2) -> 
         vee1, TypeAndEff (((Relation (join_R re1 re2)), (join_Eff eff1 eff2)))
      | TypeAndEff (Table _, _), TypeAndEff (Table _, _) -> prop vee1 vee2
      | TypeAndEff ((Lst lst1), eff1), TypeAndEff ((Lst lst2), eff2) -> 
         let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
         let lst1'', lst2'' = prop_Lst prop lst1' lst2' in
         let _, lst2'' = alpha_rename_Lsts lst2 lst2'' in
         TypeAndEff ((Lst lst1''), eff1), TypeAndEff ((Lst lst2''), (join_Eff eff1 eff2))
      | TypeAndEff ((Ary ary1), eff1), TypeAndEff ((Ary ary2), eff2) -> 
         TypeAndEff ((Ary ary1), eff1), TypeAndEff ((Ary (join_Ary ary1 ary2)), join_Eff eff1 eff2)
      | _, _ -> vee1, join_VE vee1 vee2 in
      (vars1, (rl1', vee1')), (vars2, (rl2', vee2'))
    and cons_temp_lst_Lst ve ((l,e) as vars, (rl,vee)) = 
      let eff' = match extract_eff vee with 
        | (Effect _) as e  -> e
        | _ -> EffBot
      in
      let v, _ = match ve with
        | TEBot -> Bot, EffBot
        | TypeAndEff (v, e) -> v, e
        | TETop -> Top, EffTop 
      in 
      let vee' = if is_bot_R rl then bot_shape_VE vee else 
                   (match v with
                    | Relation r -> let re' = alpha_rename_R r "cur_v" e in
                                   TypeAndEff ((Relation re'), eff')
                    | _ -> TypeAndEff (v, eff'))
      in (vars, (rl, vee'))
    and item_shape_Lst (_, (_, vee)) (vars, (rl, _)) =
      let vee' = bot_shape_VE vee in
      (vars, (rl, vee'))
    and bot_shape_Lst (vars, (rl, vee)) = 
      (vars, (rl, bot_shape_VE vee))
    (*
      *******************************
      ** Abstract domain for Tuple **
      *******************************
    *)
    and alpha_rename_Tuple u prevar var =
      List.map (fun v1 -> alpha_rename_VE v1 prevar var) u
    and replace_Tuple u var x =
      List.map (fun v1 -> replace_VE v1 var x) u
    and sat_equal_Tuple u x = 
      let inds = List.length u |> first_n in
      zip_list inds u |>
      List.fold_left (fun curr_bool (i, ve) ->
        let x_i = x^"."^(string_of_int i) in
        curr_bool && sat_equal_VE ve x_i) true
    and equal_Tuple u var = 
      let inds = List.length u |> first_n in
      zip_list inds u |> 
      List.map (fun (i, ve) ->
        let var_i = var^"."^(string_of_int i) in
        equal_VE ve var_i)
    and init_Tuple l = 
      Tuple (List.init l (fun _ -> TEBot))
    and join_Tuple u1 u2 =
      List.map2 (fun v1 v2 -> join_VE v1 v2) u1 u2
    and meet_Tuple u1 u2 =
      List.map2 (fun v1 v2 -> meet_VE v1 v2) u1 u2
    and leq_Tuple u1 u2 =
      List.fold_left2 (fun b v1 v2 -> b && leq_VE v1 v2) true u1 u2
    and eq_Tuple u1 u2 =
      List.fold_left2 (fun b v1 v2 -> b && eq_VE v1 v2) true u1 u2
    and wid_Tuple u1 u2 =
      List.map2 (fun v1 v2 -> wid_VE v1 v2) u1 u2
    and arrow_Tuple var u v' = 
      List.map (fun v -> arrow_VE var v v') u
    and stren_Tuple u ae = 
      List.map (fun v -> stren_VE v ae) u
    and add_tuple_item_Tuple u v =
      v :: u
    and proj_Tuple u vars = 
      List.map (fun v -> proj_VE v vars) u
    and forget_Tuple var u =
      List.map (fun v -> forget_VE var v) u
    and bot_shape_Tuple u = 
      List.map (fun v -> bot_shape_VE v) u
    and get_tuple_list u = 
      u
    and get_vars_Tuple var u =
      List.map (fun i -> var^"."^(string_of_int i)) (List.length u |> first_n)
    and only_shape_Tuple u = 
      List.fold_right (fun ve is_bot -> if is_bot then only_shape_VE ve else is_bot) u true
    and get_ae_from_Tuple u =
      List.fold_right (fun ve ae -> join_V (get_ae_from_v (extract_v ve)) Bot) u Bot

    (*
      ***************************************
      ** Abstract domain for Execution Map **
      ***************************************
      *)
    let meet_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
      NodeMap.merge (fun n v1 v2 -> Some (meet_VE v1 v2)) m1 m2
    let join_M (m1: exec_map_t) (m2: exec_map_t) : exec_map_t =
      NodeMap.union (fun n v1 v2 -> join_VE v1 v2) m1 m2
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
        let v = wid_VE v1 v2 in
        if !debug && get_label_snode n = "VN: 137;z25.z132.." then
          Format.fprintf Format.std_formatter "sem line 1230 @,v: @[%a@]@, @,v1: @[%a@]@, @,v2: @[%a@]@" 
          pr_value_and_eff v pr_value_and_eff v1 pr_value_and_eff v2;
        v
        ) m1 m2
    let leq_M (m1: exec_map_t) (m2: exec_map_t) : bool =
      NodeMap.for_all (fun n v1 (*untie to node -> value*) -> 
        NodeMap.find_opt n m2 |> Opt.map (fun v2 -> leq_VE v1 v2) |>
        Opt.get_or_else (is_bot_VE v1)) m1
    let eq_PM (m1:exec_map_t) (m2:exec_map_t) =
      NodeMap.for_all (fun n v1 (*untie to node -> value*) ->
        NodeMap.find_opt n m2 |> Opt.map (
        fun v2 -> 
          let l = get_label_snode n in
          if eq_VE v1 v2 then true else 
          raise (Pre_Def_Change ("Predefined node changed at " ^ l))
         )
      |> Opt.get_or_else (v1 = v1)) m1
    let top_M m = NodeMap.map (fun a -> TETop) m
    let array_M env m = 
      let n_make = construct_vnode env "Array.make" create_empty_trace in
      let s_make = construct_snode create_empty_trace n_make in
      let te_make = (* make *)
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
          let t' = Table (construct_table (create_singleton_trace_call_loc var_e) 
                            ((init_VE_v (Relation rm)), 
                            (init_VE_v (Ary ary)) |> construct_fout create_empty_trace)) in
          Table (construct_table create_empty_trace 
                   ((init_VE_v (Relation rl)), 
                   (init_VE_v t') |> construct_fout create_empty_trace)) in
        init_VE_v t
      in
      let n_len = construct_vnode env "Array.length" create_empty_trace in
      let s_len = construct_snode create_empty_trace n_len in
      let te_len = (* len *)
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
        let t = Table (construct_table (create_singleton_trace_call_loc var_l) 
                 ((init_VE_v (Ary ary)), 
                 (init_VE_v (Relation rlen) |> construct_fout create_empty_trace)))
        in
        init_VE_v t
      in
      let n_get = construct_vnode env "Array.get" create_empty_trace in
      let s_get = construct_snode create_empty_trace n_get in
      let te_get = (* get *)
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
          let t' = Table (construct_table (create_singleton_trace_call_loc var_zi) 
                            ((init_VE_v (Relation rm)), 
                            (init_VE_v (Relation rr)) |> construct_fout create_empty_trace)) in
          Table (construct_table (create_singleton_trace_call_loc var_l) 
                   ((init_VE_v (Ary ary)), 
                   (init_VE_v t') |> construct_fout create_empty_trace)) in
        init_VE_v t
      in
      let n_set = construct_vnode env "Array.set" create_empty_trace in
      let s_set = construct_snode create_empty_trace n_set in
      let te_set = (* set *)
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
          let t' = Table (construct_table (create_singleton_trace_call_loc var_e) 
                            ((init_VE_v (Relation rri)), 
                            (init_VE_v (Tuple rrr)) |> construct_fout create_empty_trace)) in
          let t'' = Table (construct_table (create_singleton_trace_call_loc var_zi) 
                             ((init_VE_v (Relation rm)), 
                             (init_VE_v t') |> construct_fout create_empty_trace)) in
          Table (construct_table (create_singleton_trace_call_loc var_l) 
                   ((init_VE_v (Ary ary)), 
                   (init_VE_v t'') |> construct_fout create_empty_trace)) in
        init_VE_v t
      in
      let m' = 
        m |> NodeMap.add s_make te_make |> NodeMap.add s_len te_len |> NodeMap.add s_get te_get
      |> NodeMap.add s_set te_set
      in
      let env' = 
        env |> VarMap.add "Array.make" (n_make, false) |> VarMap.add "Array.length" (n_len, false) |> VarMap.add "Array.get" (n_get, false)
      |> VarMap.add "Array.set" (n_set, false)
      in
      pre_def_func := List.append !pre_def_func ["Array.make"; "Array.length"; "Array.get"; "Array.set"];
      env', m'
    let list_M env m = 
      let n_len = construct_vnode env "List.length" create_empty_trace in
      let s_len = construct_snode create_empty_trace n_len in
      let te_len = (* len *)
        (* len |-> zl: { v: 'a List (l, e) | len: [| l>=0; |] item: true } 
           -> { v: Int | [| v=l |] } *)
        let var_l = "zl" in
        let var_len = "l" in
        (* let var_i = "i" in *)
        let list = 
          let l, e = "l", "e" in
          let rl = top_R Plus |> op_R "" var_len "0" Ge true in
          let ve = TETop in
          (l, e), (rl, ve)
        in
        (* let rl = op_R var_i "0" Ge true llen |> op_R var_i var_len Lt true in *)
        let rlen = equal_R (top_R Plus) "l" in
        let t = Table (construct_table (create_singleton_trace_call_loc var_l) 
                 ((init_VE_v (Lst list)), 
                 (init_VE_v (Relation rlen)) |> construct_fout create_empty_trace))
        in init_VE_v t
      in
      let n_hd = construct_vnode env "List.hd" create_empty_trace in
      let s_hd = construct_snode create_empty_trace n_hd in
      let te_hd = (* hd *)
        (* hd |-> zh: { v: 'a List (l, e) | l: [| l>=0; |] e: true } 
           -> true  *)
        let var_h = "zh" in
        (* let var_i = "i" in *)
        let list = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l in
          let ve = TETop in
          (l, e), (rl, ve)
        in
        let tr = Top in
        let t =
          Table (construct_table (create_singleton_trace_call_loc var_h) 
                   ((init_VE_v (Lst list)), 
                   (init_VE_v tr) |> construct_fout create_empty_trace)) in
        init_VE_v t
      in
      let n_tl = construct_vnode env "List.tl" create_empty_trace in
      let s_tl = construct_snode create_empty_trace n_tl in
      let te_tl = (* tl *)
        (* tl |-> zt: { v: Int List (l, e) | l: [| l>=0; |] e: true } -> 
           { v: Int List (l1, e1) | l1: [| l1=l-1; |] e1: e } *)
        let var_t = "zt" in
        let var_len, var_e = "l", "e" in
        (* let var_i = "i" in *)
        let list1 = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l in
          let ve = TETop in
          (l, e), (rl, ve)
        in
        let list2 = 
          let l, e = "l'", "e'" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l |> op_R l var_len "1" Minus true in
          (* let re = replace_R (rl |> op_R "" "cur_v" var_e Eq true) "cur_v" e in *)
          let ve = TETop in
          (l, e), (rl, ve)
        in
        let t =
          Table (construct_table (create_singleton_trace_call_loc var_t) 
                   ((init_VE_v (Lst list1)), 
                   (init_VE_v (Lst list2)) |> construct_fout create_empty_trace)) in
        init_VE_v t
      in
      let n_cons = construct_vnode env "List.cons" create_empty_trace in
      let s_cons = construct_snode create_empty_trace n_cons in
      let te_cons = (* cons *)
        (* cons |-> zc: true -> xs: { v: Int List (l, e) | l: [| l>=0; |] e: true } -> 
           { v: Int List (l1, e1) | l': [| l1=l+1; |] e': e ⊔ zc |] }
         *)
        let var_c = "zc" in
        let ve = TETop in
        let var_len, var_e = "l", "e" in
        (* let var_i = "i" in *)
        let list1 = 
          let l, e = "l", "e" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "0" Ge true) "cur_v" l in
          let ve = TETop in
          (l, e), (rl, ve)
        in
        let var_l = "xs" in
        let list2 = 
          let l, e = "l'", "e'" in
          let rl = replace_R (top_R Plus |> op_R "" "cur_v" "1" Ge true) "cur_v" l |> op_R l var_len "1" Plus true in
          let ve = TETop in
          (l, e), (rl, ve)
        in
        let t =
          let t' = Table (construct_table (create_singleton_trace_call_loc var_l) 
                            ((init_VE_v (Lst list1)), 
                            (init_VE_v (Lst list2)) |> construct_fout create_empty_trace)) in
          Table (construct_table (create_singleton_trace_call_loc var_c) 
                   (ve, (init_VE_v t') |> construct_fout create_empty_trace)) in
        init_VE_v t
      in
      let m' = 
        m |> NodeMap.add s_hd te_hd |> NodeMap.add s_len te_len |> NodeMap.add s_tl te_tl
      |> NodeMap.add s_cons te_cons
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
              let n_var = construct_vnode env var create_empty_trace in
              let s_var = construct_snode create_empty_trace n_var in
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
              let te_var = init_VE_v t_var in
              let env' = env |> VarMap.add var (n_var, false) in
              let m' = m |> NodeMap.add s_var te_var in
              m', env') !pre_vars (m, env)
      in env', m'

  end
