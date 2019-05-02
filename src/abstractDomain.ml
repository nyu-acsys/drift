
open Apron
open Syntax

(*
 *******************************
 ** Abstract domain for value **
 *******************************
 *)
type var = string
(* Define Abstract Domain Functor*)
module type ManagerType =
  sig
    type t
    val man: t Manager.t
  end

module BoxManager: ManagerType =
  struct
    type t = Box.t
    let man = Box.manager_alloc ()
  end

module type AbstractDomainType =
  sig
    type t
    val leq: t -> t -> bool
    val bot: unit -> t
    val init_c: value -> t
    val join: t -> t -> t
    val meet: t -> t -> t
    val getMan: unit -> 'a Manager.t
    val alpha_rename: t -> var -> var -> t
    val getBool: t -> bool
  end

module MakeAbstractDomainValue (Man: ManagerType): AbstractDomainType =
  struct
    type t = Abstract1.t (*Could be parsed constraints or given initial*)
    (*val initialize : 'a list -> unit*)
    let init_c c = match c with
      | true | false -> (* TODO: Add Bool domain*)
      | _ -> let var_v = "v" |> Var.of_string in
        let env = Environment.make [|var_v|] [||] in
        let expr = Linexpr1.make env in
        let cons = Lincons1.make expr Lincons1.EQ in
        let tab = Lincons1.array_make env 1 in
        Lincons1.array_set tab 1 cons;
        (* Creation of abstract value v = c *)
        Abstract1.of_lincons_array Man.man env tab
    let leq v1 v2 = Abstract1.is_leq Man.man v1 v2
    (* let rec prop v1 v2 =  *)
    let bot () = function x -> Abstract1.bottom Man.man
    let join v1 v2 = Abstract1.join Man.man v1 v2
    let meet v1 v2 = Abstract1.meet Man.man v1 v2
    let getMan () = function x -> Man.man
    let alpha_rename v prevar var =
        let (int_vars, real_vars) = Environment.vars v in
        let var_new = var |> Var.of_string in
        let int_vars_new = Array.map
            (fun x -> if x.to_string = var then Var.of_string var else x )
            int_vars in
        Abstract1.rename_array Man.man v int_vars_new real_vars
    let getBool v = if v = True then true else false
  end

(*Where to make MakeAbstractDomain(BoxManager).t?*)

  (*
   ****************************************
   ** Abstract domain for all semantics **
   ****************************************
   *)
type BaseType = Int | Bool
type loc = int

module VarMap = Map.Make(struct
    type t = var
    let compare = compare
end)

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
    let abstractValue = MakeAbstractDomain(BoxManager)
    type abstractRelation_t = BaseType * abstractValue.t
    let alpha_rename_R r prevar var = let (a, v) = r in
        (a, abstractValue.alpha_rename v prevar var)
    let is_bool_R a = match a with
        | Int, _ -> false
        | Bool, v1 -> true
    let bool_comp a b = match a with
        | Int, _ -> false
        | Bool, bl -> abstractValue.getBool bl && b
    let init_R_c (c:value) = match c with
        | true | false -> (Bool, abstractValue.init_c c)
        | _ -> (Int, abstractValue.init_c c)
    let join_R (a1:abstractRelation_t) (a2:abstractRelation_t) =
        match a1, a2 with
          | (Int,v1), (Int,v2) -> (Int, abstractValue.join v1 v2)
          | (Bool,v1), (Bool,v2) -> (Bool, abstractValue.join v1 v2)
          | _, _ -> Invalid_argument "Base Type not equal"
    let meet_R (a1:abstractRelation_t) (a2:abstractRelation_t) =
        match a1, a2 with
          | (Int,v1), (Int,v2) -> (Int, abstractValue.meet v1 v2)
          | (Bool,v1), (Bool,v2) -> (Bool, abstractValue.meet v1 v2)
          | _, _ -> Invalid_argument "Base Type not equal"
    let leq_R (a1:abstractRelation_t) (a2:abstractRelation_t) =
        match a1, a2 with
          | (Int,v1), (Int,v2) -> abstractValue.leq v1 v2
          | (Bool,v1), (Bool,v2) -> abstractValue.leq v1 v2
          | _, _ -> false
    let add_R a var1 var2 = abstractValue.addCons a var1 var2
    (*
     *******************************
     ** Abstract domain for Table **
     *******************************
     *)
     type node_t = EN of env_t * loc (*N = E x loc*)
     and env_t = node_t VarMap.t (*E = Var -> N*)
     type table_t = var * value_t * value_t
     let init_T var = (var, Bot, Bot)
     let dx_T t = let (z, vi, vo) = t in z
     let io_T t = let (z, vi, vo) = t in vi, vo
     let alpha_rename_T t prevar var = let (z, vi, vo) = t in
        if z = var then (prevar, alpha_rename_V vi z prevar, alpha_rename_V vo z prevar)
        else
            (z, alpha_rename_V vi prevar var, alpha_rename_V vo prevar var)
     let alpha_rename t1 t2 = let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then t1, t2
        else (*a renaming*)
         let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
         in ("z", v1i, v1o'), ("z", v2i, v2o')
     let join_T (t1:table_t) (t2:table_t) :table_t = let t =
        let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
        if z1 = z2 then (z1, join_V v1i v2i, join_V v1o v2o) else (*a renaming*)
            let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
            in ("z", join_V v1i v2i, join_V v1o' v2o')
        in t
     let meet_T (t1:table_t) (t2:table_t) :table_t = let t =
         let (z1, v1i, v1o) = t1 and (z2, v2i, v2o) = t2 in
         if z1 = z2 then (z1, meet_V v1i v2i, meet_V v1o v2o) else (*a renaming*)
             let v1o' = alpha_rename_V v1o z1 "z" and v2o' = alpha_rename_V v2o z2 "z"
             in ("z", meet_V v1i v2i, meet_V v1o' v2o')
         in t
     let leq_T (t1:table_t) (t2:table_t) = match t1, t2 with
        | (z, v1i, v1o), (z, v2i, v2o) -> (leq_V v1i v2i) && (leq_V v1o v2o)
        | _, _ -> false
     (*
      ***************************************
      ** Abstract domain for Execution Map **
      ***************************************
      *)
      type exec_map_t = value_t NodeMap.t
      let meet_M (m1:exec_map_t) (m2:exec_map_t) = let m =
        NodeMap.for_all (fun n v1 -> NodeMap.find_opt n m2
        |> Opt.map (fun v2 -> meet_V v1 v2) |>
        Opt.get_or_else (v1 = Bot)) m1
        in m
      let join_M (m1:exec_map_t) (m2:exec_map_t) = let m =
        NodeMap.for_all (fun n v1 -> NodeMap.find_opt n m2
        |> Opt.map (fun v2 -> join_V v1 v2) |>
        Opt.get_or_else (v1 = Bot)) m1
        in m
      let leq_M (m1:exec_map_t) (m2:exec_map_t) =
        NodeMap.for_all (fun n v1 (*untie to node -> value*) ->
        NodeMap.find_opt n m2 |> Opt.map (fun v2 -> leq_V v1 v2) |>
        Opt.get_or_else (v1 = Bot)) m1
      (*
       *******************************
       ** Abstract domain for Values **
       *******************************
       *)
      type value_t =
        | Bot
        | Top
        | Relation of abstractRelation_t
        | Table of table_t
      let alpha_rename_V v prevar var = match v with
        | Bot -> Bot
        | Relation r -> alpha_rename_R r prevar var
        | Table t -> alpha_rename_T t prevar var
        | Top -> Top
      let init_V_c (c:value) = Relation (init_R_c c)
      let join_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
        | Bot, v | v, Bot -> v
        | Relation r1, Relation r2 -> Relation (join_R r1 r2)
        | Table t1, Table t2 -> Table (join_T t1 t2)
        | _, _ -> Top
      let meet_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
        | Top, v | v, Top -> v
        | Relation r1, Relation r2 -> Relation (meet_R r1 r2)
        | Table t1, Table t2 -> Table (meet_T t1 t2)
        | _, _ -> Bot
      let leq_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
        | Bot, _ -> true
        | _, Top -> true
        | Relation r1, Relation r2 -> leq_R r1 r2
        | Table t1, Table t2 -> leq_T t1 t2
        | _, _ -> false
      let add_V (v1:value_t) (v2:value_t) :value_t = match v1, v2 with
          | Bot, _ -> v2
          | _, Top -> v1
          | Relation r1, Relation r2 -> add_R r1 r2
          | Table t1, Table t2 -> leq_T t1 t2
          | _, _ -> v2
      let is_table v = match v with
        | Table _ -> true
        | _ -> false
      let is_bool_V v = match v with
        | Relation r -> is_bool_R r
        | _ -> false
  end
