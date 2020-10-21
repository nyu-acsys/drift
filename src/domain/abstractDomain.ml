
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
      
(*let parse_domain = function
  | "Box" -> Box.manager_alloc () |> Obj.magic
  | "Oct" -> Oct.manager_alloc () |> Obj.magic
  | "Polka_st" -> Polka.manager_alloc_strict() |> Obj.magic
  | "Polka_ls" -> Polka.manager_alloc_loose () |> Obj.magic
  | "Polka_eq" -> Polka.manager_alloc_equalities() |> Polka.manager_of_polka_equalities |> Obj.magic
  | "Ppl_st" -> Ppl.manager_alloc_strict() |> Ppl.manager_of_ppl_strict |> Obj.magic
  | "Ppl_gd" ->  Ppl.manager_alloc_grid() |> Ppl.manager_of_ppl_grid |> Obj.magic
  | "Polka_gd" -> let man_polka = Polka.manager_alloc_strict() in
      let man_pplgrid = Ppl.manager_alloc_grid() in
      PolkaGrid.manager_alloc man_polka man_pplgrid |> PolkaGrid.manager_of_polkagrid |> Obj.magic
  | _ -> raise (Invalid_argument "Incorrect domain specification")*)
(* Define Abstract Domain Module*)

module type Domain =
  sig
    type t
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
      Syntax.binop -> int -> t -> t
    val uoperator :
      string ->
      string ->
      Syntax.unop -> int -> t -> t
    val assign :
      string ->
      string ->
      string -> Syntax.binop -> t -> t
    val print_abs : Format.formatter -> t -> unit
    (*val print_env : Format.formatter -> t -> unit*)
    val derived : string -> t -> t
    val sat_cons : t -> string -> bool
  end
        
module type DomainManager =
  sig
    type t
    val man : t Apron.Manager.t
  end
      
module BaseDomain(Manager : DomainManager) : Domain = 
  struct
    open Manager
    type t = Manager.t Abstract1.t 

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
            (*let expr = vl ^ "=" ^ vr in
            let tab = Parser.tcons1_of_lstring env' [expr] in*)
            Hashtbl.add cache (env, vl, vr) (env', tab); env', tab)
            ()
        in
        (* Creation of abstract value vl = vr *)
        (* (if !debug then
        begin
          Format.printf "\n\n = operation \n";
          Format.printf "%s \n" expr;
          Format.printf "Before: " ;
          Abstract1.print Format.std_formatter v;
          Format.printf "\n";
        end); *)
        let v' = Abstract1.change_environment man v env' false in
        let res = Abstract1.meet_lincons_array man v' tab in
        
        (* (if !debug then
        begin
          Format.printf "result: " ;
          Abstract1.print Format.std_formatter res;
          Format.printf "\n";
        end); *)
        Abstract1.minimize_environment man res
          (* res *)
          
    let licons_ref = 
      let env = Environment.make [||] [||] in
      let ary = Lincons1.array_make env 0 in
      ref ary

    let licons_earray env (vars : string list) = 
      let tset_size = 
        ThresholdsSetType.cardinal !thresholdsSet in
      let mult_lst = Util.extract 2 vars in
      let size =
        (List.length vars) * 4 * tset_size + (List.length mult_lst * 4) in
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
        let eq = lvar^" >= "^rvar in (* v1 >= v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" < "^rvar in (* v1 < v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;
        let eq = lvar^" > "^rvar in (* v1 > v2 *)
        Lincons1.array_set thehold_ary (!idx2) (Parser.lincons1_of_string env eq); 
        idx2 := !idx2 + 1;) mult_lst;
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
       licons_earray env lst

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
          (let expr = vres ^ " = " ^ vl ^ " " ^ temp ^ " " ^ vr in
          let tab = Parser.tcons1_of_lstring env [expr] in
          Abstract1.meet_tcons_array man v' tab)
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
      | Not -> failwith "Not yet implemented"

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
    let from_int c =
      D1.from_int c, D2.from_int c
        
    let is_bot (v1, v2) =
      D1.is_bot v1 && D2.is_bot v2
        
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

    
module OctDomain = BaseDomain(struct
  type t = Oct.t
  let man = Oct.manager_alloc ()
end)

module PolkaStrictDomain = BaseDomain(struct
  type t = Polka.strict Polka.t
  let man = Polka.manager_alloc_strict ()
end)
                                
module PolkaLooseDomain = BaseDomain(struct
  type t = Polka.loose Polka.t
  let man = Polka.manager_alloc_loose ()
end)

module OctPolkaDomain = ProductDomain(OctDomain)(PolkaLooseDomain)
    
let abstractValue = match !domain with
| "Oct" -> (module OctDomain : Domain)
| "Polka_st" -> (module PolkaStrictDomain : Domain)
| "Polka_ls" -> (module PolkaLooseDomain : Domain)
| "OctPolka" -> (module OctPolkaDomain : Domain)
| _ -> failwith ("unsupported abstract domain " ^ !domain)
       
module AbstractValue = (val (abstractValue) : Domain)
    
(* Domain Specification
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
    type t = Elina_poly.strict
    let man = Elina_poly.manager_alloc_strict() |> Elina_poly.manager_of_elina_poly_strict
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
    type t = Elina_poly.loose
    let man = Elina_poly.manager_alloc_loose () |> Elina_poly.manager_of_elina_poly_loose
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

module PolkaGridManager: ManagerType =
  struct
    (* 
      Reduced product of NewPolka polyhedra (strict) and PPL grids
    *)
    type t = (Polka.strict) PolkaGrid.t
    let man = let man_polka = Polka.manager_alloc_strict() in
      let man_pplgrid = Ppl.manager_alloc_grid() in
      PolkaGrid.manager_alloc man_polka man_pplgrid |> PolkaGrid.manager_of_polkagrid
  end



module AbstractValue = (val (!domain |> parse_domain)) *)
