
open Apron
open Syntax
open Util
open Config

(*
 *******************************
 ** Abstract domain for value **
 *******************************
 *)
type var = string
type loc = int
(* Define Abstract Domain Functor*)
module type ManagerType =
  sig
    type t
    val man: t Manager.t
  end

module type AbstractDomainType =
  sig
    type t
    val lc_env: t -> t -> t * t
    val leq: t -> t -> bool
    val init_c: int -> t
    val join: t -> t -> t
    val meet: t -> t -> t
    val alpha_rename: t -> var -> var -> t
    val forget_var: var -> t -> t
    val equal_var: t -> var -> var -> t
    val widening: t -> t -> t
    val expanding: var -> t -> t -> t
    val operator: var -> var -> binop -> t
    val print_abs: Format.formatter -> t -> unit
  end

module MakeAbstractDomainValue (Man: ManagerType): AbstractDomainType =
  struct
    type t = Man.t Abstract1.t (*Could be parsed constraints or given initial*)
    let init_c c = let var_v = "cur_v" |> Var.of_string in
        let env = Environment.make [|var_v|] [||] in
        let expr = "cur_v=" ^ (string_of_int c) in
        let tab = Parser.lincons1_of_lstring env [expr] in
        (* Creation of abstract value v = c *)
        Abstract1.of_lincons_array Man.man env tab
    let lc_env v1 v2 = 
      let env1 = Abstract1.env v1 in
      let env2 = Abstract1.env v2 in
      let env = Environment.lce env1 env2 in
      let v1' = Abstract1.change_environment Man.man v1 env false in
      let v2' = Abstract1.change_environment Man.man v2 env false in
      (v1', v2')
    let leq v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      Abstract1.is_leq Man.man v1' v2'
    let join v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      Abstract1.join Man.man v1' v2'
    let meet v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      (if !debug then
        (Abstract1.print Format.std_formatter v1;
        Abstract1.print Format.std_formatter v2;)
        );
      Abstract1.meet Man.man v1' v2'
    let alpha_rename v prevar var =
        let (int_vars, real_vars) = Environment.vars (Abstract1.env v) in
        let int_vars_new = Array.map
            (fun x -> if (Var.to_string x) = prevar then Var.of_string var else x )
            int_vars in
        Abstract1.rename_array Man.man v int_vars int_vars_new
    let forget_var var v =
        let vari = var |> Var.of_string in
        let arr = [|vari|] in
        (if !debug then
        Abstract1.print Format.std_formatter v);
        Abstract1.forget_array Man.man v arr true
    let equal_var v vl vr = let var_l = vl |> Var.of_string and var_r = vr |> Var.of_string in
        let env = Environment.make [|var_l; var_r|] [||] in
        let expr = vl ^ "=" ^ vr in
        let tab = Parser.lincons1_of_lstring env [expr] in
        (* Creation of abstract value vl = vr *)
        (if !debug then
        Abstract1.print Format.std_formatter v);
        let res = Abstract1.of_lincons_array Man.man env tab in
        res
    let widening v1 v2 = let v1',v2' = lc_env v1 v2 in
      Abstract1.widening Man.man v1' v2'
    let expanding var vi v = let temp = (alpha_rename vi "cur_v" var) in
      (if !debug then Abstract1.print Format.std_formatter temp;
      Abstract1.print Format.std_formatter v);
      let res = join v temp in
      res 
    let operator vl vr op = 
      let var_l = vl |> Var.of_string and var_r = vr |> Var.of_string in
      let env = Environment.make [|var_l; var_r|] [||] in
      let temp = (match op with
        | Plus -> "+"
        | Ge -> ">="
      )in
      let expr = vl ^ temp ^ vr in
      let tab = Parser.lincons1_of_lstring env [expr] in
      (* Creation of abstract value vl op vr *)
      Abstract1.of_lincons_array Man.man env tab
    let print_abs ppf a = Abstract1.print ppf a
  end

(*Domain Specification*)
module BoxManager: ManagerType =
  struct
    type t = Box.t
    let man = Box.manager_alloc ()
  end

module OctManager: ManagerType =
  struct
    type t = Oct.t
    let man = Oct.manager_alloc ()
  end


  module AbstractValue = MakeAbstractDomainValue(BoxManager)