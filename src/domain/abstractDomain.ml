
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
    val eq: t -> t -> bool
    val init_c: int -> t
    val top: t
    val bot: t
    val is_bot: t -> bool
    val join: t -> t -> t
    val meet: t -> t -> t
    val alpha_rename: t -> var -> var -> t
    val forget_var: var -> t -> t
    val project_other_vars: t -> var array -> t
    val equal_var: t -> var -> var -> t
    val widening: t -> t -> t
    val operator: var -> var -> binop -> int -> t -> t
    val print_abs: Format.formatter -> t -> unit
    val print_env: Format.formatter -> t -> unit
    val derived: string -> t -> t
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
      let res = Abstract1.is_leq Man.man v1' v2' in
      res
    let eq v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      Abstract1.is_eq Man.man v1' v2'
    let join v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      (if !debug then
        begin
        Format.printf "\n\nJoin\n";
        Abstract1.print Format.std_formatter v1;
        Format.printf "\n ^with \n";
        Abstract1.print Format.std_formatter v2;
        Format.printf "\nEnv: ";
        Environment.print Format.std_formatter (Abstract1.env v1');
        Format.printf "\n";
        end
      );
      let res = Abstract1.join Man.man v1' v2' in
      (if !debug then
        begin
          Format.printf "result: ";
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
      end);
      (* Abstract1.minimize_environment Man.man res *)
      res
    let meet v1 v2 =
      let v1',v2' = lc_env v1 v2 in
      let res = Abstract1.meet Man.man v1' v2' in
      res
    let alpha_rename v prevar var =
        (if !debug then
        begin
          Format.printf "\n\nA rename";
          Format.printf "\n %s -> %s \n" prevar var;
          Abstract1.print Format.std_formatter v;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter (Abstract1.env v);
          Format.printf "\n";
        end
        );
        let (int_vars, real_vars) = Environment.vars (Abstract1.env v) in
        (* Check previous variable exists or not *)
        let pre_b = Array.fold_left (fun b x -> prevar = Var.to_string x || b) false int_vars in
        let v' = if pre_b = false then v (* ignore rename if prevar does not exist *)
          else
          begin
            (* Check new variable exists or not *)
            let v_b = Array.fold_left (fun b x -> var = Var.to_string x || b) false int_vars in
            if v_b = true then 
              begin
                (* check prevar and newvar has the same constraint or not *)
                (* TODO: If not the same, return bottom *)
                (* If same, project prevar *)
                let int_vars_new = Array.fold_left (fun ary x -> if prevar <> Var.to_string x then Array.append ary [|x|] else ary) [||] int_vars in
                let env' = Environment.make int_vars_new real_vars in
                Abstract1.change_environment Man.man v env' true
              end
            else
              (* new var does not exist, change environment *)
              begin
              let var_V = Var.of_string var in
              let int_vars_new = Array.map (fun x -> if prevar = Var.to_string x then var_V else x) int_vars in
              Abstract1.rename_array Man.man v int_vars int_vars_new
              end
          end
        in
        (if !debug then 
        begin
          Format.printf ("Result: ");
          Abstract1.print Format.std_formatter v';
          Format.printf "\n";
        end);
        (* Abstract1.minimize_environment Man.man v' *)
        v'
    let forget_var var v =
        (if !debug then
          begin
          Format.printf "\n\nProjection %s at:\n" var;
          Abstract1.print Format.std_formatter v;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter (Abstract1.env v);
          Format.printf "\n";
          end
        );
        let (int_vars, real_vars) = Environment.vars (Abstract1.env v) in
        let var_b = Array.fold_left (fun b x -> var = Var.to_string x || b) false int_vars in
        let res = if var_b = false then v
        else
        begin
        let vari = var |> Var.of_string in
        let arr = [|vari|] in
        let v' = Abstract1.forget_array Man.man v arr false in
        (* Abstract1.minimize_environment Man.man v' *)
        v'
        end 
        in
        (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end);
        res
    let project_other_vars v vars = 
        let env = Abstract1.env v in
        (if !debug then
          begin
          Format.printf "\n\nRemain [ %a] at:\n" pr_ary vars;
          Abstract1.print Format.std_formatter v;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter env;
          Format.printf "\n";
          end
        );
        let vars' = Array.append vars [|"cur_v"|] in
        let (int_vars, real_vars) = Environment.vars env in
        let int_vars' = Array.fold_left (fun arry var -> let str = Var.to_string var in 
          if Array.mem str vars' then Array.append arry [|var|] else arry) [||] int_vars 
        in
        let env' = Environment.make int_vars' real_vars in
        let res = Abstract1.change_environment Man.man v env' false in
        (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end);
        res
    let top = let env = Environment.make [||] [||] in
        Abstract1.top Man.man env
    let bot = let env = Environment.make [||] [||] in
        Abstract1.bottom Man.man env
    let is_bot v = Abstract1.is_bottom Man.man v
    let equal_var v vl vr = let var_l = vl |> Var.of_string and var_r = vr |> Var.of_string in
        let env = Environment.make [|var_l; var_r|] [||] in
        let expr = vl ^ "=" ^ vr in
        let tab = Parser.tcons1_of_lstring env [expr] in
        (* Creation of abstract value vl = vr *)
        (if !debug then
        begin
          Format.printf "\n\n = operation \n";
          Format.printf "%s \n" expr;
          Format.printf "Before: " ;
          Abstract1.print Format.std_formatter v;
          Format.printf "\n";
        end);
        let env_v = Abstract1.env v in
        let env' = Environment.lce env env_v in
        let v' = Abstract1.change_environment Man.man v env' false in
        let res = Abstract1.meet_tcons_array Man.man v' tab in
        (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end);
        (* Abstract1.minimize_environment Man.man res *)
        res
    let widening v1 v2 = let v1',v2' = lc_env v1 v2 in
      let res =
        Abstract1.widening Man.man v1' v2'
      in
      (* Abstract1.minimize_environment Man.man res *)
      res
    let make_var var = 
      try let _ = int_of_string var in None
      with e -> Some (var |> Var.of_string)
    let operator vl vr op cons v = 
      (if !debug then
      begin
        Format.printf "\n\nOperator abs\n";
        Format.printf "%s %s %s\n" vl (string_of_op op) vr
      end);
      let var_v = "cur_v" |> Var.of_string in
      let env = (match (make_var vl, make_var vr) with
      | None, Some var_r -> Environment.make [|var_r|] [||]
      | Some var_l, None -> Environment.make [|var_l;|] [||]
      | Some var_l, Some var_r -> Environment.make [|var_l; var_r|] [||]
      | None, None -> Environment.make [||] [||])
      |> Environment.lce (Environment.make [|var_v|] [||])in
      (if !debug then 
      begin
        Format.printf "Env: ";
        Environment.print Format.std_formatter env;
        Format.printf "\n";
        Format.printf "Before: " ;
        Abstract1.print Format.std_formatter v;
        Format.printf "\n";
      end);
      let temp = string_of_op op in
      let env_v = Abstract1.env v in
      let env' = Environment.lce env env_v in
      let v' = Abstract1.change_environment Man.man v env' false in
      let res = 
        if cond_op op = true then
          (
          let vt = if temp = "!=" then (* '!=' not support by apron, use vl < vr join vl > vr *)
            begin
              let expr1 = vl ^ "<" ^ vr in
              let expr2 = vl ^ ">" ^ vr in
              let tab = Parser.tcons1_of_lstring env [expr1] in
              let vlt' = Abstract1.meet_tcons_array Man.man v' tab in
              let tab = Parser.tcons1_of_lstring env [expr2] in
              let vgt' = Abstract1.meet_tcons_array Man.man v' tab in
              Abstract1.join Man.man vlt' vgt'
            end
          else
            begin
              let expr = vl ^ temp ^ vr in
              let tab = Parser.tcons1_of_lstring env [expr] in
              Abstract1.meet_tcons_array Man.man v' tab
            end
          in
          if cons = -1 then vt
          else (* Bool value *)
            let exprv = "cur_v=" ^ (string_of_int cons) in
            let tab = Parser.tcons1_of_lstring env [exprv] in
            Abstract1.meet_tcons_array Man.man vt tab
          )
        else (* Int value *)
          (let expr = "cur_v=" ^ vl ^ temp ^ vr in
            let tab = Parser.tcons1_of_lstring env [expr] in
          Abstract1.meet_tcons_array Man.man v' tab)
      in
      (* Creation of abstract value vl op vr *)
      (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end);
      (* Abstract1.minimize_environment Man.man res *)
      res
    let print_abs ppf v = Abstract1.print ppf v
    let print_env ppf v = let env = Abstract1.env v in
        Environment.print ppf env
    let derived expr v = 
      (if !debug then
      begin
        Format.printf "\n\nStrengthen derived\n";
        Format.printf "%s \n" expr;
        Format.printf "Before: " ;
        Abstract1.print Format.std_formatter v;
        Format.printf "\n";
      end);
      let res = try 
        let env = Abstract1.env v in
        let tab = Parser.tcons1_of_lstring env [expr] in
        Abstract1.meet_tcons_array Man.man v tab 
        with
        _ -> v
      in
      (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end);
      res
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

module PolkaStManager: ManagerType =
  struct 
    (* 
      Convex polyhedra are defined by the conjunction of a set of linear constraints of the form 
      a_0*x_0 + ... + a_n*x_n + b >= 0 or a_0*x_0 + ... + a_n*x_n + b > 0 
      where a_0, ..., a_n, b, c are constants and x_0, ..., x_n variables.
    *)
    type t = Polka.strict
    let man = Polka.manager_alloc_strict() |> Polka.manager_of_polka_strict
  end

module PolkaEqManager: ManagerType =
  struct
    (* 
      Linear equalities are conjunctions of linear equalities of the form a_0*x_0 + ... + a_n*x_n + b = 0.
    *)
    type t = Polka.equalities
    let man = Polka.manager_alloc_equalities() |> Polka.manager_of_polka_equalities
  end

module PolkaLsManager: ManagerType =
  struct
    (* 
      Loose polyhedra cannot have strict inequality constraints like x>0. 
      They are algorithmically more efficient (less generators, simpler normalization). 
    *)
    type t = Polka.loose
    let man = Polka.manager_alloc_loose() |> Polka.manager_of_polka_loose
  end

module PplGridManager: ManagerType =
  struct
    (* 
      Linear congruences
    *)
    type t = Ppl.grid
    let man = Ppl.manager_alloc_grid() |> Ppl.manager_of_ppl_grid
  end

module PplStrictManager: ManagerType =
  struct
    (* 
      wrapper around the Parma Polyhedra
    *)
    type t = Ppl.strict
    let man = Ppl.manager_alloc_strict() |> Ppl.manager_of_ppl_strict
  end

let parse_domain = function
  | "Box" -> (module (MakeAbstractDomainValue(BoxManager)): AbstractDomainType)
  | "Oct" -> (module (MakeAbstractDomainValue(OctManager)): AbstractDomainType)
  | "Polka_st" -> (module (MakeAbstractDomainValue(PolkaStManager)): AbstractDomainType)
  | "Polka_ls" -> (module (MakeAbstractDomainValue(PolkaLsManager)): AbstractDomainType)
  | "Polka_eq" -> (module (MakeAbstractDomainValue(PolkaEqManager)): AbstractDomainType)
  | "Ppl_st" -> (module (MakeAbstractDomainValue(PplStrictManager)): AbstractDomainType)
  | "Ppl_gd" -> (module (MakeAbstractDomainValue(PplGridManager)): AbstractDomainType)
  | _ -> raise (Invalid_argument "Incorrect domain specification")

module AbstractValue = (val (!domain |> parse_domain))
