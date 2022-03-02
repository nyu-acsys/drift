
open Apron
open DriftSyntax
open Util
open Config
open Format

(*
 *******************************
 ** Abstract domain for value **
 *******************************
 *)
type var = string
      
(* Define Abstract Domain Module*)

module type Domain =
  sig
    type t
    val name : string
    val from_int : int -> t
    val is_bot : t -> bool
    val contains_var : string -> t -> bool
    val leq : t -> t -> bool
    val eq : t -> t -> bool
    val join :
      t -> t -> t
    val meet :
      t -> t -> t
    val alpha_rename :
      t -> string -> string -> t
    val forget_var : string -> t -> t
    val project_other_vars :
      t -> string list -> t
    val top : t
    val bot : t
    val equal_var :
      t -> string -> string -> t
    val widening :
      t -> t -> t
    val operator :
      string ->
      string ->
      string ->
      DriftSyntax.binop -> int -> t -> t
    val uoperator :
      string ->
      string ->
      DriftSyntax.unop -> int -> t -> t
    val assign :
      string ->
      string ->
      string -> DriftSyntax.binop -> t -> t
    val print_abs : Format.formatter -> t -> unit
    (*val print_env : Format.formatter -> t -> unit*)
    val derived : string -> t -> t
    val sat_cons : t -> string -> bool
  end
        
module type DomainManager =
  sig
    type t
    val man : t Apron.Manager.t
    val name : string
  end
      
module BaseDomain(Manager : DomainManager) : Domain =
  struct
    open Manager
    type t = Manager.t Abstract1.t
    let name = Manager.name

    let max_size = 150
    let max_length = 15
    let from_int c =
      let var_v = "cur_v" |> Var.of_string in
      let env = Environment.make [|var_v|] [||] in
      let expr = "cur_v=" ^ (string_of_int c) in
      let tab = Parser.lincons1_of_lstring env [expr] in
      (* Creation of abstract value v = c *)
      Abstract1.of_lincons_array man env tab

    let is_bot v = Abstract1.is_bottom man v

    let contains_var var v = 
      let env = Abstract1.env v in
      Environment.mem_var env (Var.of_string var) 

    let lc_env v1 v2 = 
      (* let v1, v2 = (Abstract1.minimize_environment man v1), (Abstract1.minimize_environment man v2) in *)
      let env1 = Abstract1.env v1 in
      let env2 = Abstract1.env v2 in
      let comp = Environment.compare env1 env2 in
      let v1', v2' = 
        if comp = 0 then v1, v2 else
        if comp = -1 then 
        Abstract1.change_environment man v1 env2 false, v2
        else if comp = 1 then v1, Abstract1.change_environment man v2 env1 false
        else
        let env = Environment.lce env1 env2 in
        Abstract1.change_environment man v1 env false,
        Abstract1.change_environment man v2 env false in
      (v1', v2')

    let leq v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      let res = Abstract1.is_leq man v1' v2' in
      res

    let eq v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      Abstract1.is_eq man v1' v2'

    let leq_without_lcenv v1 v2 = Abstract1.is_leq man v1 v2

    let eq_without_lcenv v1 v2 = Abstract1.is_eq man v1 v2

    let join v1 v2 = 
      let v1',v2' = lc_env v1 v2 in
      if leq_without_lcenv v1' v2' then
        v2
      else if leq_without_lcenv v2' v1' then
        v1
      else
      (* (if !debug then
        begin
        Format.printf "\n\nJoin\n";
        Abstract1.print Format.std_formatter v1';
        Format.printf "\n ^with \n";
        Abstract1.print Format.std_formatter v2';
        Format.printf "\nEnv: ";
        Environment.print Format.std_formatter (Abstract1.env v1');
        Format.printf "\n";
        flush stdout;
        Format.print_flush ();
        end
      ); *)
      let res = Abstract1.join man v1' v2' in
      (* (if !debug then
        begin
          Format.printf "result: ";
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
      end); *)
      if !domain <> "Oct" && ((Abstract1.size man res > max_size) ||
        (Tcons1.array_length (Abstract1.to_tcons_array man res)) >= max_length)
        then delay_wid := 0;
      Abstract1.minimize_environment man res
      (* res *)

    let meet v1 v2 =
      let v1',v2' = lc_env v1 v2 in
      if leq_without_lcenv v1' v2' then
        v1
      else if leq_without_lcenv v2' v1' then
        v2
      else
      (* (if !debug then
        begin
        Format.printf "\n\nMeet\n";
        Abstract1.print Format.std_formatter v1;
        Format.printf "\n ^with \n";
        Abstract1.print Format.std_formatter v2;
        Format.printf "\nEnv: ";
        Environment.print Format.std_formatter (Abstract1.env v1');
        Format.printf "\n";
        flush stdout;
        Format.print_flush ();
        end
      ); *)
      let res = Abstract1.meet man v1' v2' in
      (* (if !debug then
        begin
          Format.printf "result: ";
          Abstract1.print Format.std_formatter (Abstract1.minimize_environment man res);
          Format.printf "\n";
      end); *)
      if !domain <> "Oct" && ((Abstract1.size man res > max_size) ||
        (Tcons1.array_length (Abstract1.to_tcons_array man res)) >= max_length)
        then delay_wid := 0;
      Abstract1.minimize_environment man res
      (* res *)

    let alpha_rename v prevar var =
        (* (if !debug then
        begin
          Format.printf "\n\nA rename";
          Format.printf "\n %s -> %s \n" prevar var;
          Abstract1.print Format.std_formatter v;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter (Abstract1.env v);
          Format.printf "\n";
        end
        ); *)
        if is_bot v then v else
        if prevar = var 
        (* Check previous variable exists or not *)
          || contains_var prevar v = false then v else
        let (int_vars, real_vars) = Environment.vars (Abstract1.env v) in
        let v' = if contains_var var v then
            (* Check new variable exists or not *) 
              begin
                (* check prevar and newvar has the same constraint or not *)
                (* TODO: If not the same, return bottom *)
                (* If same, project prevar *)
                let int_vars_new =
                  Array.fold_left (fun ary x -> if prevar <> Var.to_string x then Array.append ary [|x|] else ary) [||] int_vars
                in
                let env' = Environment.make int_vars_new real_vars in
                Abstract1.change_environment man v env' false
              end
            else
              (* new var does not exist, change environment *)
              begin
              let var_V = Var.of_string var in
              let int_vars_new = Array.map (fun x -> if prevar = Var.to_string x then var_V else x) int_vars in
              Abstract1.rename_array man v int_vars int_vars_new
              end
        in
        (* (if !debug then 
        begin
          Format.printf ("Result: ");
          Abstract1.print Format.std_formatter v';
          Format.printf "\n";
        end); *)
        Abstract1.minimize_environment man v'
          (* v' *)

    let forget_var var v =
        (* (if !debug then
          begin
          Format.printf "\n\nProjection %s at:\n" var;
          Abstract1.print Format.std_formatter v;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter (Abstract1.env v);
          Format.printf "\n";
          end
        ); *)
        if is_bot v then v else
        if contains_var var v = false then v else
        let res =  
           begin
            let vari = var |> Var.of_string in
            let arr = [|vari|] in
            let v' = Abstract1.forget_array man v arr false in
            Abstract1.minimize_environment man v'
          end 
        in
        (* (if !debug then
        begin
          Format.printf "result: %b" var_b;
          Abstract1.print Format.std_formatter res;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter (Abstract1.env res);
          Format.printf "\n";
        end); *)
        res

    let project_other_vars v vars = 
        if is_bot v then v else
        let env = Abstract1.env v in
        (* (if !debug then
          begin
          Format.printf "\n\nRemain [ %a] at:\n" pr_ary vars;
          Abstract1.print Format.std_formatter v;
          Format.printf "\nEnv: ";
          Environment.print Format.std_formatter env;
          Format.printf "\n";
          end
        ); *)
        let vars' = "cur_v" :: vars  in
        let (int_vars, real_vars) = Environment.vars env in
        let int_vars' = Array.fold_right (fun var arry -> let str = Var.to_string var in 
          if List.mem str vars' then Array.append arry [|var|] else arry) int_vars [||]
        in
        if int_vars' = int_vars then v else
        let env' = Environment.make int_vars' real_vars in
        let res = Abstract1.change_environment man v env' false in
        (* (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end); *)
        Abstract1.minimize_environment man res

    let top =
      let env = Environment.make [||] [||] in
      Abstract1.top man env

    let bot =
      let env = Environment.make [||] [||] in
      Abstract1.bottom man env
      
    let cache = Hashtbl.create 50
    let one = Coeff.s_of_int 1
    let zero = Coeff.s_of_int 0
    let mone = Coeff.s_of_int (-1)
    
    let equal_var v vl vr =
      if is_bot v then v else
        let var_l = vl |> Var.of_string in
        let var_r = vr |> Var.of_string in
        let env = Abstract1.env v in
        let env', tab =
          Hashtbl.find_opt cache (env, vl, vr) |>
          Opt.lazy_get_or_else (fun () ->
            let ary = match Environment.mem_var env var_l, Environment.mem_var env var_r with
            | true, true -> [||]
            | true, false -> [|var_r|]
            | false, true -> [|var_l|]
            | _ -> [|var_l; var_r|]
            in
            let env' = Environment.add env ary [||] in
            let tab =
              let linc = Linexpr1.make env' |> fun e -> Linexpr1.set_list e [(one, var_l); (mone, var_r)] (Some zero); e in
              let eq = Lincons1.make linc Lincons1.EQ in
              let ea = Lincons1.array_make env' 1 in
              Lincons1.array_set ea 0 eq; ea
            in
            Hashtbl.add cache (env, vl, vr) (env', tab); env', tab) ()
        in
        let v' = Abstract1.change_environment man v env' false in
        let res = Abstract1.meet_lincons_array man v' tab in
        Abstract1.minimize_environment man res

    let licons_ref = 
      let env = Environment.make [||] [||] in
      let ary = Lincons1.array_make env 0 in
      ref ary

    let licons_earray env (vars : string list) complex = 
      if complex = false then 
       thresholdsSet:= !thresholdsSet |> ThresholdsSetType.remove 111 |> ThresholdsSetType.remove 101;
      let tset_size = 
        ThresholdsSetType.cardinal !thresholdsSet in
      let mult_lst = Util.extract 2 vars in
      let size =
        let second = if complex then 10 else 5 in
        (List.length vars) * 4 * tset_size + (List.length mult_lst * second) in
      let thehold_ary = Lincons1.array_make env size in
      let idx2 = ref 0 in
      List.iter (fun var -> let _ = ThresholdsSetType.map (fun i -> 
      let eq = var^" <=" ^ (string_of_int i) in (* v <=  threshold_const *)
      Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
      idx2 := !idx2 + 1;
      let eq = var^" >=" ^ (string_of_int i) in (* v >=  threshold_const *)
      Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
      idx2 := !idx2 + 1;
      let eq = var^" <" ^ (string_of_int i) in (* v <  threshold_const *)
      Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
      idx2 := !idx2 + 1;
      let eq = var^" >" ^ (string_of_int i) in (* v >  threshold_const *)
      Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
      idx2 := !idx2 + 1;
      i) !thresholdsSet in ()) vars;
      List.iter (fun lst -> 
        let lvar, rvar = List.nth lst 0, List.nth lst 1 in
        let eq = lvar^" <= "^rvar in (* v1 <= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" < "^rvar in (* v1 < v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" > "^rvar in (* v1 > v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" >= "^rvar in (* v1 >= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" >= "^rvar^"+1" in (* v1 >= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        if complex then
        (let eq = lvar^" >= 2*"^rvar in (* v1 >= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = "3*"^lvar^" <= "^rvar^"+3" in (* v1 <= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = "2*"^lvar^" <= "^rvar^"+1" in (* v1 >= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" <= "^rvar^"+1" in (* v1 <= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" <= 2*"^rvar in (* v1 <= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;);
        ) mult_lst;
        thehold_ary

    let generate_threshold_earray env = 
      if Environment.size env = 0 then Lincons1.array_make env 0 
      (* else if Environment.mem_var env (Var.of_string "cur_v") then !licons_ref  *)
      else
        let int_vars, _ = Environment.vars env in
        let lst = Array.fold_left (fun lst item -> let var = Var.to_string item in
        var :: lst) [] int_vars in
        (* if Environment.mem_var env (Var.of_string "min") && Environment.mem_var env (Var.of_string "max") then
          (Environment.print Format.std_formatter env;
          licons_earray ary (Some "max"))
        else  *)
      let complex = if Array.length int_vars < 10 then true else false in
      licons_earray env lst complex

    let widening v1 v2 = 
      if is_bot v2 then v1 else
      if eq v1 v2 then v2 else
      let v1', v2' = lc_env v1 v2 in
      let res = 
        if !use_threshold then
          begin
          Abstract1.widening_threshold man v1' v2' (generate_threshold_earray (Abstract1.env v1'))
          end
        else
          Abstract1.widening man v1' v2'
      in
      Abstract1.minimize_environment man res
      (* res *)

    let make_var var = 
      try let _ = int_of_string var in None
      with e -> Some (var |> Var.of_string)

    (*let op_cache = Hashtbl.create 100 |> Obj.magic*)
    let operator vres vl vr op cons v =
      (* (if !debug then
      begin
        Format.printf "\n\nOperator abs\n";
        Format.printf "%s %s %s\n" vl (string_of_op op) vr
      end); *)
      if is_bot v then v else
      let vres = if vres = "" then "cur_v" else vres in
      let var_v = vres |> Var.of_string in
      let env = (match (make_var vl, make_var vr) with
      | None, Some var_r -> Environment.make [|var_r|] [||]
      | Some var_l, None -> Environment.make [|var_l;|] [||]
      | Some var_l, Some var_r -> Environment.make [|var_l; var_r|] [||]
      | None, None -> Environment.make [||] [||])
      |> Environment.lce (Environment.make [|var_v|] [||])
      in
      (* (if !debug then 
      begin
        Format.printf "Env: ";
        Environment.print Format.std_formatter env;
        Format.printf "\n";
        Format.printf "Before: " ;
        Abstract1.print Format.std_formatter v;
        Format.printf "\n";
         end); *)
      
      let temp = string_of_op op in
      let env_v = Abstract1.env v in
      let env' = Environment.lce env env_v in
      let v' = Abstract1.change_environment man v env' false in
      let res =
        if cond_op op = true then
          (
           let vt =
             if temp = "!=" then (* '!=' not support by apron, use vl < vr join vl > vr *)
               begin
                 let expr1 = vl ^ "<" ^ vr in
                 let expr2 = vl ^ ">" ^ vr in
                 let tab1 = Parser.tcons1_of_lstring env [expr1] in
                 let tab2 = Parser.tcons1_of_lstring env [expr2] in
                 let vlt' = Abstract1.meet_tcons_array man v' tab1 in
                 let vgt' = Abstract1.meet_tcons_array man v' tab2 in
                 Abstract1.join man vlt' vgt'
               end
             else
               begin
                 (* if temp = "<=" then
                    let expr = "min <= max" in
                    let vmin = Var.of_string "min" in
                    let ary = [|Var.of_string "min"; Var.of_string "max"|] in
                    let env = Environment.make ary [||] in
                    let tab = Parser.tcons1_of_lstring env [expr] in
                    Abstract1.meet_tcons_array man v' tab
                    else *)
                 let expr = vl ^ " " ^ temp ^ " " ^ vr in
                 let tab = Parser.tcons1_of_lstring env [expr] in
                 Abstract1.meet_tcons_array man v' tab
               end
           in
           if cons = -1 then vt
           else (* Bool value *)
             let exprv = vres ^ " = " ^ (string_of_int cons) in
             let tab = Parser.tcons1_of_lstring env [exprv] in
             Abstract1.meet_tcons_array man vt tab
          )
        else (* Int value *)
          begin
          let expr = vres ^ " = " ^ vl ^ " " ^ temp ^ " " ^ vr in
          let tab = Parser.tcons1_of_lstring env [expr] in
          Abstract1.meet_tcons_array man v' tab
          end
      in 
      (*let mk_res =
        Hashtbl.find_opt op_cache (env', vl, vr, op, cons) |>
        Opt.lazy_get_or_else (fun () -> let f = mk_res_fn () in Hashtbl.add op_cache (env', vl, vr, op, cons) f; f) ()
      in
      let res = mk_res v' in*)
      (* Creation of abstract value vl op vr *)
      (* (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end); *)
      Abstract1.minimize_environment man res
        (* res *)

    let operator x1 x2 x3 x4 x5 = measure_call "AbstractValue.operator" (operator x1 x2 x3 x4 x5)
    let uoperator vres ve op cons v =
      if is_bot v then v else
      match op with
      | UMinus -> let v' = operator vres "0" ve Minus cons v in
         alpha_rename v' ve "cur_v"

    let assign vres vl vr op v = 
      let vres = if vres = "" then "cur_v" else vres in
      let var_v = vres |> Var.of_string in
      let env = (match (make_var vl, make_var vr) with
      | None, Some var_r -> Environment.make [|var_r|] [||]
      | Some var_l, None -> Environment.make [|var_l;|] [||]
      | Some var_l, Some var_r -> Environment.make [|var_l; var_r|] [||]
      | None, None -> Environment.make [||] [||])
      |> Environment.lce (Environment.make [|var_v|] [||]) in
      let temp = string_of_op op in
      let env_v = Abstract1.env v in
      let env' = Environment.lce env env_v in
      let v' = Abstract1.change_environment man v env' false in
      let res = 
        if cond_op op = true then
          v
        else (* Int value *)
          (let expr = vl ^ " " ^ temp ^ " " ^ vr in
            let tab = Parser.texpr1_of_string env expr in
          Abstract1.assign_texpr man v' var_v tab None)
      in
      (* Creation of abstract value vl op vr *)
      (* (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end); *)
      Abstract1.minimize_environment man res
      (* res *)

    let print_abs ppf v = Abstract1.print ppf v

    let print_env ppf v = let env = Abstract1.env v in
        Environment.print ppf env

    let derived expr v = 
      (* (if !debug then
      begin
        Format.printf "\n\nStrengthen derived\n";
        Format.printf "%s \n" expr;
        Format.printf "Before: " ;
        Abstract1.print Format.std_formatter v;
        Format.printf "\n";
      end); *)
      let res = try 
        let env = Abstract1.env v in
        let tab = Parser.tcons1_of_lstring env [expr] in
        Abstract1.meet_tcons_array man v tab 
        with
        _ -> v
      in
      (* (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end); *)
      res

    let sat_cons v var =
      if contains_var var v && contains_var "cur_v" v then
        let env = Abstract1.env v in
        let expr = "cur_v = " ^ var in
        let tab = Parser.tcons1_of_string env expr in
        Abstract1.sat_tcons man v tab
      else false
  end

(*module BaseManager : DomainManager =
  struct
    type t = Oct.t
    let man = !domain |> parse_domain
  end*)

module ProductDomain(D1 : Domain)(D2: Domain) : Domain =
  struct
    type t = D1.t * D2.t
    let name = D1.name ^ " * " ^ D2.name
    let from_int c =
      D1.from_int c, D2.from_int c
        
    let is_bot (v1, v2) =
      D1.is_bot v1 || D2.is_bot v2
        
    let contains_var x (v1, v2) =
      D1.contains_var x v1 || D2.contains_var x v2
        
    let leq (v11, v12) (v21, v22) =
      D1.leq v11 v21 && D2.leq v12 v22
        
    let eq (v11, v12) (v21, v22) =
      D1.eq v11 v21 && D2.leq v12 v22
        
    let join (v11, v12) (v21, v22) =
      D1.join v11 v21, D2.join v12 v22
        
    let meet (v11, v12) (v21, v22) =
      D1.meet v11 v21, D2.meet v12 v22

    let alpha_rename (v1, v2) x y =
      D1.alpha_rename v1 x y, D2.alpha_rename v2 x y

    let forget_var x (v1, v2) =
      D1.forget_var x v1, D2.forget_var x v2
        
    let project_other_vars (v1, v2) xs =
      D1.project_other_vars v1 xs, D2.project_other_vars v2 xs

    let top = D1.top, D2.top
    let bot = D1.bot, D2.bot

    let equal_var (v1, v2) x y =
      D1.equal_var v1 x y, D2.equal_var v2 x y

    let widening (v11, v12) (v21, v22) =
      D1.widening v11 v21, D2.widening v12 v22

    let operator vres vl vr op cons (v1, v2) =
      D1.operator vres vl vr op cons v1,
      D2.operator vres vl vr op cons v2

    let uoperator vres ve op cons (v1, v2) =
      D1.uoperator vres ve op cons v1,
      D2.uoperator vres ve op cons v2

    let assign vres vl vr op (v1, v2) =
      D1.assign vres vl vr op v1,
      D2.assign vres vl vr op v2

    let print_abs ppf (v1, v2) =
      D1.print_abs ppf v1;
      Format.print_string " && ";
      D2.print_abs ppf v2
        
    let derived expr (v1, v2) =
      D1.derived expr v1,
      D2.derived expr v2
        
    let sat_cons (v1, v2) var =
      D1.sat_cons v1 var ||
      D2.sat_cons v2 var
  end

module FiniteValueDomain: Domain = struct
  (** values of all vars we're tracking, all vars we're tracking                      *)
  (** (fvm, tracked) where fvm = FiniteValueMap                                       *)
  (** domain(fvm) <= domain(tracked). Items missing from fvm (but present in tracked) *)
  (** are considered to be top (i.e. (-inf,inf))                                      *)

  type t = { fvm: IntSet.t StringMap.t; tracked: StringSet.t }
  let name = "FiniteValue"
  let from_int c = { fvm = StringMap.singleton "cur_v" (IntSet.singleton c); tracked = StringSet.singleton "cur_v" }
  let is_bot v = StringMap.exists (fun _ vals -> IntSet.is_empty vals) v.fvm
  let contains_var var v = StringSet.mem var v.tracked

  let max_cardinality = 32

  let print_abs ppf v = begin
    fprintf ppf "@[{";
    pp_print_list
      ~pp_sep:(fun ppf () -> pp_print_custom_break ~fits:(";", 1, "") ~breaks:("",0,"") ppf)
      (fun ppf var ->
         fprintf ppf "%s =@ " var;
         match StringMap.find_opt var v.fvm with
         | None ->
           pp_print_string ppf "top"
         | Some vals ->
           fprintf ppf "@[<hov 2>[";
           pp_print_list
             ~pp_sep:(fun ppf () -> pp_print_custom_break ~fits:(",",1,"") ~breaks:("",0,"") ppf)
             pp_print_int
             ppf
             (IntSet.elements vals);
           fprintf ppf "]@]";
          )
      ppf
      (StringSet.elements v.tracked);
    fprintf ppf "}@]";
  end

  let leq v1 v2 =
    let pointwise_leq var =
      match (StringMap.find_opt var v1.fvm, StringMap.find_opt var v2.fvm) with
      | _         , None       -> true
      | None      , Some _     -> false
      | Some vals1, Some vals2 -> IntSet.subset vals1 vals2
    (* the comparison HAS to be over the v2 since missing elements are considered top.                             *)
    (* as an example: ({ } <= { a = {1,2} }) = false. This wouldn't be the case if the comparison was made over v1 *)
    in StringSet.for_all pointwise_leq v2.tracked

  let eq v1 v2 = StringMap.equal (=) v1.fvm v2.fvm

  let join v1 v2 =
    let fvm =
      StringMap.merge (fun _var vals1 vals2 -> match vals1, vals2 with
      | _, None -> None
      | None, _ -> None
      | Some x, Some y -> Some (IntSet.union x y)
      ) v1.fvm v2.fvm
    in
    let tracked = StringSet.union v1.tracked v2.tracked in
    { fvm; tracked }

  let meet v1 v2 =
    let fvm =
      StringMap.merge (fun _var vals1 vals2 -> match vals1, vals2 with
      | None, None -> None
      | Some x, None -> Some x
      | None, Some y -> Some y
      | Some x, Some y -> Some (IntSet.inter x y)
      ) v1.fvm v2.fvm
    in
    let tracked = StringSet.union v1.tracked v2.tracked in
    { fvm; tracked }

  let alpha_rename v old_var new_var =
    let tracked = v.tracked |> StringSet.remove old_var |> StringSet.add new_var in
    let fvm = match StringMap.find_opt old_var v.fvm with
    | None -> v.fvm
    | Some vs -> v.fvm |> StringMap.remove old_var |> StringMap.add new_var vs
    in
    { fvm; tracked }

  let forget_var var v = { fvm = StringMap.remove var v.fvm; tracked = StringSet.remove var v.tracked }

  let project_other_vars v vars =
    let fvm =
      vars
      |> List.to_seq
      |> Seq.filter_map (fun var -> StringMap.find_opt var v.fvm |> Opt.map (fun vals -> (var, vals)))
      |> StringMap.of_seq
    in
    let tracked = StringSet.of_list vars in
    { fvm; tracked }

  let top = { fvm = StringMap.empty; tracked = StringSet.empty }
  let bot = { fvm = StringMap.singleton "cur_v" IntSet.empty; tracked = StringSet.singleton "cur_v" }

  (* add the constraint: var1 = var2 *)
  let equal_var v var1 var2 =
    let fvm = match StringMap.find_opt var1 v.fvm, StringMap.find_opt var2 v.fvm with
    | None, None -> v.fvm
    | Some vals1, None -> StringMap.add var2 vals1 v.fvm
    | None, Some vals2 -> StringMap.add var1 vals2 v.fvm
    | Some vals1, Some vals2 ->
      let vals = IntSet.inter vals1 vals2 in
      v.fvm |> StringMap.add var1 vals |> StringMap.add var2 vals
    in
    let tracked = v.tracked |> StringSet.add var1 |> StringSet.add var2 in
    { fvm; tracked }

  (* join old & new, replace by top every var whose values exceed max_cardinality *)
  let widening old_v new_v =
    let {fvm; tracked} = join old_v new_v in
    let fvm' =
      StringMap.filter
        (fun _var vals -> IntSet.cardinal vals <= max_cardinality)
        fvm
    in
    {fvm = fvm'; tracked}

  (*
    take a look at BaseDomain
    looks like `cons` indicates whether this is a true or false case of a boolean (1, 0 respectively) or if neither, then -1
  *)
  let operator result_var left_var right_var binop cons dom =
    let result_var = if result_var = "" then "cur_v" else result_var in
    let left_vals_opt = StringMap.find_opt left_var dom.fvm in
    let right_vals_opt = StringMap.find_opt right_var dom.fvm in
    let result_vals_opt = match left_vals_opt, right_vals_opt with
    (* if either is top, then the result is top *)
    | None, _ | _, None -> None
    | Some left_vals, Some right_vals ->
      let merge_vals l r = match binop with
        | Plus       -> l + r
        | Minus      -> l - r
        | Mult       -> l * r
        | Div        -> l / r
        | Mod | Modc -> l mod r
        | Eq         -> int_of_bool (l = r)
        | Ne         -> int_of_bool (l <> r)
        | Lt         -> int_of_bool (l < r)
        | Gt         -> int_of_bool (l > r)
        | Le         -> int_of_bool (l <= r)
        | Ge         -> int_of_bool (l >= r)
        | And        -> l * r (* @Check: should we do something special (eg. return bot) if `l` or `r` are not 0 or 1 *)
        | Or         -> l + r
        | Cons       -> failwith "unsupported"
        | Seq        -> failwith "unsupported"
      in
      Some (set_union_with merge_vals left_vals right_vals)
    in
    let fvm' =
      match result_vals_opt with
      | Some result_vals ->
        if result_var = "cur_v" then
          (* @Check
             It makes sense to me to overwrite the values if we're evaluating the current expression. Is this okay?
             - ketan
          *)
          StringMap.add result_var result_vals dom.fvm
        else
          StringMap.update
            result_var
            (fun old_vals -> Some (Option.fold ~none:result_vals ~some:(IntSet.union result_vals) old_vals)) dom.fvm
      | None ->
        dom.fvm
    in
    let tracked' = StringSet.add result_var dom.tracked in
    let dom' = { fvm = fvm'; tracked = tracked' } in
    (* if !debug then *)
    (*   Format.printf "OPERATOR: (expr: [%s]@ :=@ [%s] [%s] [%s])@ [cons: %d]@ @[<hov 2>[dom: %a]@]@ @[<hov 2>[result: %a]@]@.---@." *)
    (*     result_var *)
    (*     left_var (string_of_op binop) right_var *)
    (*     cons *)
    (*     print_abs dom *)
    (*     print_abs dom'; *)
    dom'

  let uoperator result_var var unop cons v = failwith "TODO"

  let assign result_var left_var right_var binop v = failwith "TODO"

  (* maybe rework interface? string expr -> term ; TODO check how this is being used *)
  (* 'derived' is only ever called (by der_R, it's only caller) with exprs of the form 'v1 = v2' *)
  (* so probably only do the interface for this much *)
  let derived expr v = failwith "TODO"

  (* can constraints on `var` be satisfied (assuming cur_v = var here) *)
  let sat_cons v var =
    let var_sat_cons var =
      StringMap.find_opt var v.fvm
      |> Option.fold ~none:true ~some:(fun vals -> not @@ IntSet.is_empty vals)
    in
    (var_sat_cons var && var_sat_cons "cur_v")
end
    
module OctDomain = BaseDomain(struct
  type t = Oct.t
  let man = Oct.manager_alloc ()
  let name = "Octagon"
end)

module PolkaStrictDomain = BaseDomain(struct
  type t = Polka.strict Polka.t
  let man = Polka.manager_alloc_strict ()
  let name = "PolkaStrict"
end)
                                
module PolkaLooseDomain = BaseDomain(struct
  type t = Polka.loose Polka.t
  let man = Polka.manager_alloc_loose ()
  let name = "PolkaLoose"
end)

module OctPolkaDomain = ProductDomain(OctDomain)(PolkaLooseDomain)

let abstractValue = match !domain with
| "Oct" -> (module OctDomain : Domain)
| "Polka_st" -> (module PolkaStrictDomain : Domain)
| "Polka_ls" -> (module PolkaLooseDomain : Domain)
| "OctPolka" -> (module OctPolkaDomain : Domain)
| "FinVal" -> (module FiniteValueDomain : Domain)
| _ -> failwith ("unsupported abstract domain " ^ !domain)

module AbstractValue = (val (abstractValue) : Domain)
