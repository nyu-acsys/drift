open AbstractDomain
open Syntax
open SemanticDomain
open SemanticsDomain
open SensitiveDomain
open SenSemantics
open Printer
open Util
open Config
(*
** c[M] is a single base refinement type that pulls in all the environment dependencies from M
M = n1 |-> {v: int | v >= 2}, n2 |-> z:{v: int | v >= 0} -> {v: int | v = z}, n3 |-> {v: bool | v = true}
c[M] = {v: int | v = c && n1 >= 2}

** t[M]
    t' = {v:int | v = 2}
    x in scope and M^t[x] = {v: bool | v = true}
    the scope of t is the set of all nodes in the range (i.e. codomain) of the environment component of n.
    t'[M^t] = {v: int | v = 2 && x = true}
** True False
    Using v = 1 be true once inside vt and v = 1 be false inside vf
*)

type stage =
  | Widening
  | Narrowing
  
let process = ref "Wid"

module AssertionPosMap =  Map.Make(struct
  type t = Syntax.pos
  let compare = compare
  end)

type asst_map_t = (int * int) AssertionPosMap.t

let sens : asst_map_t ref = ref AssertionPosMap.empty

let env0, m0 = 
    let enva, ma = array_M VarMap.empty (NodeMap.create 500) in
    let envb, mb = list_M enva ma in
    envb, mb

(* Create a fresh variable name *)
let fresh_z= fresh_func "z"

let arrow_V x1 x2 = measure_call "arrow_V" (arrow_V x1 x2)
let forget_V x = measure_call "forget_V" (forget_V x)
let stren_V x = measure_call "stren_V" (stren_V x)
let join_V e = measure_call "join_V" (join_V e)
let meet_V e = measure_call "meet_V" (meet_V e)
let equal_V e = measure_call "equal_V" (equal_V e)
let proj_V e = measure_call "proj_V" (proj_V e)
let sat_equal_V e = measure_call "sat_equal_V" (sat_equal_V e)

    
let rec prop (v1: value_t) (v2: value_t): (value_t * value_t) = match v1, v2 with
    | Top, Bot | Table _, Top -> Top, Top
    | Table t, Bot ->
        let t' = init_T (dx_T v1) in
        v1, Table t'
    | Relation r1, Relation r2 ->
        if leq_R r1 r2 then v1, v2 else
        Relation r1, Relation (join_R r1 r2)
    | Table t1, Table t2 ->
        let prop_table_entry cs (v1i, v1o) (v2i, v2o) =
          let _, z = cs in
          let v1ot, v2ip = 
            if (is_Array v1i && is_Array v2i) || (is_List v1i && is_List v2i) then
              let l1 = get_len_var_V v1i in
              let e1 = get_item_var_V v1i in
              let l2 = get_len_var_V v2i in
              let e2 = get_item_var_V v2i in
              let v = replace_V v1o l1 l2 in
              replace_V v e1 e2, (forget_V l1 v2i |> forget_V e1)
            else v1o, v2i
          in
          let v1i', v2i', v1o', v2o' = 
            let opt_i = false 
                (*Optimization 1: If those two are the same, ignore the prop step*)
            || (v1i <> Bot && v2i <> Bot && eq_V v2i v1i) in
            let v2i', v1i' = 
              if opt_i then v2i, v1i else 
              prop v2i v1i 
            in
            let opt_o = false
                (*Optimization 2: If those two are the same, ignore the prop step*)
            || (v2o <> Bot && v1o <> Bot && eq_V v1ot v2o)
            in
            let v1o', v2o' = 
              if opt_o then v1ot, v2o else
              prop (arrow_V z v1ot v2ip) (arrow_V z v2o v2i)
            in
            let v1o' =
              if (is_Array v1i' && is_Array v2i') || (is_List v1i' && is_List v2i') then
                let l1 = get_len_var_V v1i' in
                let e1 = get_item_var_V v1i' in
                let l2 = get_len_var_V v2i' in
                let e2 = get_item_var_V v2i' in
                let v = replace_V v1o' l2 l1 in
                replace_V v e2 e1
                else v1o'
            in
            (* if !debug then
                begin
                (if true then
                begin
                    Format.printf "\n<=== prop o ===>%s\n" z;
                    pr_value Format.std_formatter v1ot;
                    Format.printf "\n";
                    pr_value Format.std_formatter v2i;
                    Format.printf "\n";
                    pr_value Format.std_formatter (arrow_V z v1ot v2i);
                    Format.printf "\n<<~~~~>>\n";
                    pr_value Format.std_formatter (arrow_V z v2o v2i);
                    Format.printf "\n";
                end
                );
                (if true then
                begin
                    Format.printf "\n<=== res ===>%s\n" z;
                    pr_value Format.std_formatter v1o';
                    Format.printf "\n<<~~~~>>\n";
                    pr_value Format.std_formatter v2o';
                    Format.printf "\n";
                end
                );
              end; *)
            let v1o', v2o' = 
              if opt_o then v1o', v2o' else (join_V v1o v1o', join_V v2o v2o')
            in
            v1i', v2i', v1o', v2o'
          in
          (v1i', v1o'), (v2i', v2o') 
        in
        let t1', t2' =
          prop_table prop_table_entry alpha_rename_V t1 t2
        in
        Table t1', Table t2'
    | Ary ary1, Ary ary2 -> let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
        let ary' = (join_Ary ary1' ary2') in
        let _, ary2'' = alpha_rename_Arys ary2 ary'  in
        (* let _ = alpha_rename_Arys ary1 ary' in *)
        Ary ary1, Ary ary2''
    | Lst lst1, Lst lst2 -> let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
        let lst1'', lst2'' = prop_Lst prop lst1' lst2' in
        let _, lst2'' = alpha_rename_Lsts lst2 lst2'' in
        Lst lst1'', Lst lst2''
    | Ary ary, Bot -> let vars, r = ary in
        let ary' = init_Ary vars in
        v1, Ary ary'
    | Lst lst, Bot -> let vars, r = lst in
        let lst' = init_Lst vars in
        v1, Lst lst'
    | Tuple u, Bot ->  let u' = List.init (List.length u) (fun _ ->
             Bot) in
        v1, Tuple u'
    | Tuple u1, Tuple u2 -> if List.length u1 <> List.length u2 then
        raise (Invalid_argument "Prop tuples should have the same form")
        else
            let u1', u2' = List.fold_right2 (fun v1 v2 (u1', u2') -> 
                let v1', v2' = prop v1 v2 in
                (v1'::u1', v2'::u2')
            ) u1 u2 ([],[]) in
            Tuple u1', Tuple u2'
    | _, _ -> if leq_V v1 v2 then v1, v2 else v1, join_V v1 v2

let prop p = measure_call "prop" (prop p)

        
(* let lc_env env1 env2 = 
    Array.fold_left (fun a id -> if Array.mem id a then a else Array.append a [|id|] ) env2 env1 *)

(* let rec nav (v1: value_t) (v2: value_t): (value_t * value_t) = match v1, v2 with
    | Relation r1, Relation r2 -> 
            let r2' = if leq_R r1 r2 then r1 else r2
            in
            Relation r1, Relation r2'
    | Ary ary1, Ary ary2 -> let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
      let _, ary2'' = alpha_rename_Arys ary2 (join_Ary ary1' ary2') in
      Ary ary1, Ary ary2''
    | Table t1, Table t2 -> 
        let t1', t2' = alpha_rename t1 t2 in
        let (z1, v1i, v1o) = t1' and (z2, v2i, v2o) = t2' in
        let v1ot = 
            if is_Array v1i && is_Array v2i then
                let l1 = get_len_var_V v1i in
                let l2 = get_len_var_V v2i in
                replace_V v1o l1 l2
            else v1o
        in
        let p1, p2 = 
            let v1i', v2i', v1o', v2o' = 
                let v2i', v1i' = nav v2i v1i in
                let v1o', v2o' = nav (arrow_V z1 v1ot v2i) (arrow_V z1 v2o v2i) in
                let v1o', v2o' = match leq_V v1o' v1ot, leq_V v2o' v2o with
                    | false, false -> v1ot, v2o
                    | true, false -> v1o', v2o
                    | false, true -> v1ot, v2o'
                    | true, true -> v1o', v2o'
                in
                let v1o' =
                    if is_Array v1i' && is_Array v2i' then
                        let l1 = get_len_var_V v1i' in
                        let l2 = get_len_var_V v2i' in
                        replace_V v1o' l2 l1
                    else v1o'
                in
                v1i', v2i', v1o', v2o'
            in
            (v1i', v1o'), (v2i', v2o') 
        in
        let t1'' = (z1, fst p1, snd p1) and t2'' = (z2, fst p2, snd p2) in
        Table t1'', Table t2''
    | _, _ -> v1, v2 *)

let get_env_list (env: env_t) (sx: var) (m: exec_map_t) = 
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    let env_l = VarMap.bindings env in
    let helper lst (x, (n, _)) =
      let n = construct_snode sx n in
      let t = find n m in
      let lst' = if is_List t then 
        (get_item_var_V t) :: (get_len_var_V t) :: lst
      else lst
      in
      x :: lst'
    in
    List.fold_left helper [] env_l

let get_env_list e sx = measure_call "get_env_list" (get_env_list e sx)
      
      
let prop_scope (env1: env_t) (env2: env_t) (sx: var) (m: exec_map_t) (v1: value_t) (v2: value_t): (value_t * value_t) = 
    let env1 = get_env_list env1 sx m in
    let env2 = get_env_list env2 sx m in
    (* let v1'',_  = prop v1 (proj_V v2 env1)in
    let _, v2'' = prop (proj_V v1 env2) v2 in *)
    let v1', v2' = prop v1 v2 in
    let v1'' = proj_V v1' env1 in
    let v2'' = proj_V v2' env2 in
    v1'', v2''

let prop_scope x1 x2 x3 x4 x5 = measure_call "prop_scope" (prop_scope x1 x2 x3 x4 x5)

      
(** Reset the array nodes **)
let reset (m:exec_map_t): exec_map_t = NodeMap.fold (fun n t m ->
        m |> NodeMap.add n t
    ) m0 m

(* let iterUpdate m v l = NodeMap.map (fun x -> arrow_V l x v) m *)

(* let getVars env = VarMap.fold (fun var n lst -> var :: lst) env [] *)

(* let iterEnv_c env m c = VarMap.fold (fun var n a ->
    let v = NodeMap.find_opt n m |> Opt.get_or_else Top in
    let EN (env', l) = n in
    let lb = name_of_node l in
    c_V a v lb) env (init_V_c c)

let iterEnv_v env m v = VarMap.fold (fun var n a -> 
    let ai = NodeMap.find_opt n m |> Opt.get_or_else Top in
    arrow_V var a ai) env v *)

(* let optmization m n find = (* Not sound for work *)
    if !st <= 6000 then false else
    let t = find n m in
    let pre_t = find n !pre_m in
    opt_eq_V pre_t t *)

let rec list_var_item eis sx (cs: (var * loc)) m env nlst left_or_right lst_len = 
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    let vlst = find nlst m in
    match eis, left_or_right with
    | Var (x, l), true -> 
        let n = construct_vnode env l cs in
        let env1 = env |> VarMap.add x (n, false) in
        let n = construct_snode sx n in
        let t = find n m in
        let tp = cons_temp_lst_V t vlst in
        (* (if !debug then
        begin
            Format.printf "\n<---Pattern var---> %s\n" l;
            pr_value Format.std_formatter vlst;
            Format.printf "\n<<~~~~>> %s\n" l;
            pr_value Format.std_formatter t;
            pr_value Format.std_formatter tp;
            Format.printf "\n";
        end
        ); *)
        let vlst', tp' = prop vlst tp in
        let t' = extrac_item_V (get_env_list env1 sx m) tp' in
        (* (if !debug then
        begin
            Format.printf "\nRES for prop:\n";
            pr_value Format.std_formatter vlst';
            Format.printf "\n<<~~~~>>\n";
            pr_value Format.std_formatter t';
            Format.printf "\n";
        end
        ); *)
        let m' = m |> NodeMap.add n t' |> NodeMap.add nlst vlst' in
        m', env1
    | Var(x, l), false ->
        let n = construct_vnode env l cs in
        let env1 = env |> VarMap.add x (n, false) in
        let n = construct_snode sx n in
        let lst = find n m |> get_list_length_item_V in
        let t_new = reduce_len_V lst_len lst vlst in
        let _, t' = prop t_new (find n m) in
        let m' = NodeMap.add n t' m in
        m', env1
    | Const (c, l), false ->
        let n = construct_enode env l |> construct_snode sx in
        let t_new = pattern_empty_lst_V vlst in
        let _, t' = prop t_new (find n m) in
        let m' = NodeMap.add n t' m in
        m', env
    | TupleLst (termlst, l), true -> 
        let n = construct_enode env l |> construct_snode sx in
        let t = 
            let raw_t = find n m in
            if is_tuple_V raw_t then raw_t
            else 
                let u' = List.init (List.length termlst) (fun _ ->
             Bot) in Tuple u' in
        (* (if !debug then
        begin
            Format.printf "\n<---Pattern tuple---> %s\n" l;
            pr_value Format.std_formatter vlst;
            Format.printf "\n<<~~~~>> %s\n" l;
            pr_value Format.std_formatter t;
            Format.printf "\n";
        end
        ); *)
        let tlst = get_tuple_list_V t in
        let tllst = extrac_item_V (get_env_list env sx m) vlst |> get_tuple_list_V in
        let env', m', tlst', tllst' = List.fold_left2 (fun (env, m, li, llst) e (ti, tlsti) -> 
            match e with
            | Var (x, l') -> 
                let nx = construct_vnode env l' cs in
                let env1 = env |> VarMap.add x (nx, false) in
                let nx = construct_snode sx nx in
                let tx = find nx m in
                let ti', tx' = prop ti tx in
                let tlsti', ti'' = prop tlsti ti in
                let m' = m |> NodeMap.add nx tx' in
                env1, m', (join_V ti' ti'') :: li, tlsti' :: llst
            | _ -> raise (Invalid_argument "Tuple only for variables now")
        ) (env, m, [], []) termlst (zip_list tlst tllst) in
        let tlst', tllst' = List.rev tlst', List.rev tllst' in
        let t', vlst' = Tuple tlst', (cons_temp_lst_V (Tuple tllst') vlst) in
        let m'' = m' |> NodeMap. add n t'
            |> NodeMap.add nlst vlst' in
        (* (if !debug then
        begin
            Format.printf "\nRES for tuple:\n";
            pr_value Format.std_formatter vlst';
            Format.printf "\n<<~~~~>>\n";
            pr_value Format.std_formatter t';
            Format.printf "\n";
        end
        ); *)
        m'', env'
    | BinOp (bop, e1, e2, l), _ ->
        let m', env' = list_var_item e1 sx cs m env nlst true 0 in
        let m'', env'' = list_var_item e2 sx cs m' env' nlst false (lst_len + 1) in
        m'', env''
    | UnOp (uop, e1, l), true ->
        let m', env' = list_var_item e1 sx cs m env nlst true 0 in
        m', env'
    | _ -> raise (Invalid_argument "Pattern should only be either constant, variable, and list cons")

let prop_predef l v0 v = 
    let l = l |> name_of_node in
    let rec alpha_rename v0 v = 
        match v0, v with
        | Table t0, Table t -> 
            if table_isempty t || table_isempty t0  then Table t else
            let (var0, z0), (vi0, vo0) = get_full_table_T t0 in
            let (var, z), (vi, vo) = get_full_table_T t in
            let vo' = alpha_rename vo0 (alpha_rename_V vo z z0) in
            let t' = construct_table (var0,z0) (vi, vo') in
            Table t'
        | _, _ -> v
    in
    match v0, v with
    | Table t0, Table t -> 
        let cs0, _ = get_full_table_T t0 in
        let vpdef = Table (construct_table cs0 (get_table_by_cs_T cs0 t)) in
        let pdefvp = ref (construct_table cs0 (get_table_by_cs_T cs0 t)) in
        let t' = table_mapi (fun cs (vi, vo) -> 
            let _, l' = cs in
            if cs = cs0 || l' <> l then vi, vo else
            let vs = Table (construct_table cs (vi, vo)) in
            let v' = vs |> alpha_rename v0 in
            let vt, v'' = prop vpdef v' in
            pdefvp := join_V vt (Table !pdefvp) |> get_table_T;
            let vi', vo' = match alpha_rename vs v'' with
                | Table t'' -> let _, (vi, vo) = get_full_table_T t'' in (vi, vo)
                | _ -> raise (Invalid_argument "Expect predefined functions as table")
            in vi', vo') t
        in Table (
            let _, vio = get_full_table_T !pdefvp in
            t' |> update_table cs0 vio
        )
    | _, _ -> v

          
let rec step term (env: env_t) (sx: var) (cs: (var * loc)) (ae: value_t) (assertion: bool) (is_rec: bool) (m:exec_map_t) =
    let n = construct_enode env (loc term) |> construct_snode sx in
    let update n v m = NodeMap.add n v m (*(function
      | None -> v
      | Some v' -> (wid_V v' v))
        m*)
    in
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    match term with
    | Const (c, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Const ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        (* if optmization m n find then m else *)
        let t = find n m in (* M[env*l] *)
        let t' =
          if leq_V ae t then t
          else
            let ct = init_V_c c in
            let t' = join_V t ct in
            (stren_V t' ae)
        in
        m |> update n t' (* {v = c ^ aE}*)
    | Var (x, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Var ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let (nx, recnb) = VarMap.find x env in
        let envx, lx, (varcs, lcs) = get_vnode nx in
        let nx = construct_snode sx nx in
        let tx = let tx' = find nx m in
            if is_Relation tx' then 
              if sat_equal_V tx' x then tx'
              else equal_V (forget_V x tx') x (* M<E(x)>[v=E(x)] *) 
            else tx'
        in
        let t = find n m in (* M[env*l] *)
        (* (if !debug then
        begin
            Format.printf "\n<=== Prop Var %s %b ===> %s\n" x recnb lx;
            Format.printf "cs %s, %s \n" varcs lcs;
            pr_value Format.std_formatter tx;
            Format.printf "\n<<~~~~>> %s\n" l;
            pr_value Format.std_formatter t;
            Format.printf "\n<<~~~~>> %s\n" l;
            pr_value Format.std_formatter ae;
            Format.printf "\n";
        end
        ); *)
        let tx', t' =
            (* if optmization m n find && optmization m nx find then tx, t else *)
            if List.mem lx !pre_def_func then
                let tx0 = find nx m0 in
                let tx = if !sensitive then prop_predef l tx0 tx else tx in
                prop tx t
            else
                (* if recnb then prop tx t
                else *)
                    prop_scope envx env sx m tx t
        in
        (* (if !debug then
        begin
            Format.printf "\nRES for prop:\n";
            pr_value Format.std_formatter tx';
            Format.printf "\n<<~~~~>>\n";
            pr_value Format.std_formatter t';
            Format.printf "\n";
        end
        ); *)
        m |> update nx tx' |> update n (stren_V t' ae) (* t' ^ ae *)
    | App (e1, e2, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== App ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let m0 = step e1 env sx cs ae assertion is_rec m in
        let n1 = construct_enode env (loc e1) |> construct_snode sx in
        let t0 = find n1 m0 in
        let t1, m1 =
          match t0 with
          | Bot when false ->
              let te =
                let z = fresh_z () in
                init_T (z,z)
              in
              let t1 = Table te in
              t1, update n1 t1 m0
          | _ -> t0, m0
        in
        if t1 <> Bot && not @@ is_table t1 then
          (Format.printf "Error at location %s: expected function, but found %s.\n"
             (loc e1) (string_of_value t1);
           m1 |> update n Top)
        else
          let m2 = step e2 env sx cs ae assertion is_rec m1 in
          let n2 = construct_enode env (loc e2) |> construct_snode sx in
          let t2 = find n2 m2 in (* M[env*l2] *)
          (match t1, t2 with
          | Bot, _ | _, Bot -> m2
          | _, Top -> m2 |> update n Top
          | _ ->
              let t = find n m2 in
              let cs =
                if !sensitive then
                  if is_rec && is_func e1 then cs
                  else (loc e1, loc e1 |> name_of_node)
                else (dx_T t1)
              in
              let t_temp = Table (construct_table cs (t2, t)) in
              (* let var1, var2 = cs in
                 (Format.printf "\nis_rec? %b\n") is_rec;
                 (Format.printf "\nAPP cs at %s: %s, %s\n") (loc e1) var1 var2; *)
              (* (if loc e1 = "78" then
                begin
                 Format.printf "\n<=== Prop APP ===> %s\n" (loc e1);
                 pr_value Format.std_formatter t1;
                 Format.printf "\n<<~~~~>> %s\n" l;
                 pr_value Format.std_formatter t_temp;
                 Format.printf "\n";
                 end
                 ); *)
              let t1', t0 = 
                (* if optmization m2 n1 find && optmization m2 n2 find && optmization m2 n find then t1, t_temp else *)
                prop t1 t_temp
              in
              (* (if loc e1 = "78" then
                 begin
                 Format.printf "\nRES for prop:\n";
                 pr_value Format.std_formatter t1';
                 Format.printf "\n<<~~~~>>\n";
                 pr_value Format.std_formatter t0;
                 Format.printf "\n";
                 end
                 ); *)
              let t2', raw_t' = 
                (* if is_array_set e1 then
                   let t2', t' = io_T cs t0 in
                   let elt = proj_V (get_second_table_input_V t') [] in
                   join_for_item_V t2' elt, t'
                   else  *)
                io_T cs t0
              in
              let t' = get_env_list env sx m |> proj_V raw_t' in
              let res_m = m2 |> update n1 t1' |> update n2 t2' |> update n t' in
              (* if is_array_set e1 && only_shape_V t2' = false then
                 let nx = VarMap.find (get_var_name e2) env in
                 let envx, lx,_ = get_vnode nx in
                 let nx = construct_snode sx nx in
                 let tx = find nx m in
                 let tx', t2' = prop_scope envx env sx res_m t2' tx in
                 res_m |> NodeMap.add n2 t2' |> NodeMap.add nx tx'
                 else  *)
              res_m
          )
    | BinOp (bop, e1, e2, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Binop ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let n1 = construct_enode env (loc e1) |> construct_snode sx in
        let m1 =
          match bop with
          | Cons ->
              let rec cons_list_items m term = match term with
              | BinOp (Cons, e1', e2', l') ->
                  let l1, t1', m1' = cons_list_items m e1' in
                  let l2, t2', m2' = cons_list_items m1' e2' in
                  l1 + l2, join_V t1' t2', m2'
              | _ ->
                  let m' = step term env sx cs ae assertion is_rec m in
                  let n' = construct_enode env (loc term) |> construct_snode sx in
                  let t' = find n' m' in
                  1, t', m'
              in
              let len, t1', m1 = cons_list_items m e1 in
              let t1 = find n1 m1 in
              let t1', _ = prop t1 t1' in
              m1 |> update n1 t1'
          | _ -> step e1 env sx cs ae assertion is_rec m
        in
        let m2 = step e2 env sx cs ae assertion is_rec m1 in
        let t1 = find n1 m2 in
        let n2 = construct_enode env (loc e2) |> construct_snode sx in
        let t2 = find n2 m2 in
        if t1 = Bot || t2 = Bot then m2 
        else
        begin
          (*if not @@ is_List t2 then
             if not @@ (is_Relation t1) then 
               (Format.printf "Error at location %s: expected value, but found %s.\n"
                  (loc e1) (string_of_value t1))
             else (if not @@ (is_Relation t2) then
               Format.printf "Error at location %s: expected value, but found %s.\n"
                 (loc e2) (string_of_value t2));*)
          
          let bop =
            match bop, e2 with
            | Mod, Const _ -> Modc
            | _ -> bop
          in
          let t = find n m2 in
            (* if optmization m2 n1 find && optmization m2 n2 find && optmization m2 n find then m2 else *)
          let raw_t =
            match bop with
            | Cons (* list op *) ->
                list_cons_V prop t1 t2
            | And | Or (* bool op *) ->
                bool_op_V bop t1 t2
            | Modc ->
                (* {v:int | a(t) ^ v = n1 mod const }[n1 <- t1] *)
                let td = Relation (top_R bop) in
                let node_1 = e1 |> loc |> name_of_node in
                let t' = arrow_V node_1 td t1 in
                let t''' = op_V node_1 (str_of_const e2) bop t' in
                let t = get_env_list env sx m2 |> proj_V t''' in
                t
            | Seq ->
                t2
            | _ ->
                (* {v:int | a(t) ^ v = n1 op n2 }[n1 <- t1, n2 <- t2] *)
                let td = Relation (top_R bop) in
                let node_1 = e1 |> loc |> name_of_node in
                let node_2 = e2 |> loc |> name_of_node in
                let t' = arrow_V node_1 td t1 in
                let t'' = arrow_V node_2 t' t2 in
                let t''' = op_V node_1 node_2 bop t'' in
                let temp_t = get_env_list env sx m2 |> proj_V t''' in
                (* if !domain = "Box" then
                   temp_t |> der_V e1 |> der_V e2  (* Deprecated: Solve remaining constraint only for box*)
                   else  *)
                temp_t
          in
            let _, re_t =
              if is_Relation raw_t then raw_t,raw_t else 
              (* let t, raw_t = alpha_rename_Vs t raw_t in *)
              prop raw_t t
            in
            let m2, re_t =
              match bop with
              | Cons ->
                  let t1l = cons_temp_lst_V t1 re_t in
                  let t1l', re_t' = prop t1l re_t in
                  let t1' = extrac_item_V (get_env_list env sx m2) t1l' in
                  let t2', re_t'' = prop t2 re_t' in
                  m2 |> update n1 t1' |> update n2 t2', re_t'
              | Seq ->
                  let t2', re_t' = prop t2 re_t in
                  m2 |> update n2 t2', re_t' 
              | _ -> m2, re_t 
            in
            (* (if l = "73" then
                begin
                    Format.printf "\nRES for op:\n";
                    pr_value Format.std_formatter re_t;
                    Format.printf "\n";
                end
                ); *)
            (* (if string_of_op bop = ">" then
            begin
                Format.printf "\nRES for test:\n";
                pr_value Format.std_formatter re_t;
                Format.printf "\n";
            end
            ); *)
            m2 |> update n (stren_V re_t ae)
        end
    | UnOp (uop, e1, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Unop ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let n1 = construct_enode env (loc e1) |> construct_snode sx in
        let m1 = step e1 env sx cs ae assertion is_rec m in
        let t1 = find n1 m1 in
        if t1 = Bot then m1
        else
          let node_1 = e1 |> loc |> name_of_node in
          let td = Relation (utop_R uop) in
          let t = find n m1 in
          let t' = arrow_V node_1 td t1 in
          let t'' = uop_V uop node_1 t' in
          let raw_t = get_env_list env sx m1 |> proj_V t'' in
          (* if !domain = "Box" then
             temp_t |> der_V e1 |> der_V e2  (* Deprecated: Solve remaining constraint only for box*)
             else  *)
          let _, re_t =
            if is_Relation raw_t
            then raw_t, raw_t
            else prop raw_t t
          in
          m1 |> update n (stren_V re_t ae)
    | Ite (e0, e1, e2, l, asst) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Ite ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let n0 = construct_enode env (loc e0) |> construct_snode sx in
        let m0 = 
            (* if optmization m n0 find then m else  *)
            step e0 env sx cs ae assertion is_rec m in
        let t0 = find n0 m0 in
        if t0 = Bot then m0 else
        if not @@ is_bool_V t0 then m0 |> update n Top else
        begin
            let { isast = isast; ps = pos } = asst in 
            (* QUESTION: The prop is monotone and increasing, why don't we say we could earlier determine assertion failed? *)
            if assertion && isast && not (is_bool_bot_V t0) && not (is_bool_false_V t0) then print_loc pos else
            let t_true = meet_V (extrac_bool_V t0 true) ae in (* Meet with ae*)
            let t_false = meet_V (extrac_bool_V t0 false) ae in
            (if assertion && isast then 
                let i, j = AssertionPosMap.find_opt pos !sens |> Opt.get_or_else (0,0) in
                sens := AssertionPosMap.add pos ((if is_bool_bot_V t0 && is_asst_false e0 = false then i + 1 else i), j + 1) !sens);
            let t = find n m0 in
            let m1 = step e1 env sx cs t_true assertion is_rec m0 in
            let n1 = construct_enode env (loc e1) |> construct_snode sx in
            let t1 = find n1 m1 in
            (* (if !debug then
            begin
                Format.printf "\n<=== Prop then ===> %s\n" (loc e1);
                pr_value Format.std_formatter t1;
                Format.printf "\n<<~~~~>> %s\n" l;
                pr_value Format.std_formatter (find n m1);
                Format.printf "\n";
            end
               ); *)
            let t1', t' = 
                (* if optmization m1 n1 find && optmization m1 n find then t1, (find n m1) else  *)
                (* let t1, t = alpha_rename_Vs t1 t in *)
                prop t1 t in
            (* (if !debug then
            begin
                Format.printf "\nRES for prop:\n";
                pr_value Format.std_formatter t1';
                Format.printf "\n<<~~~~>>\n";
                pr_value Format.std_formatter t';
                Format.printf "\n";
            end
            );  *)
            let m2 = step e2 env sx cs t_false assertion is_rec m1 in
            let n2 = construct_enode env (loc e2) |> construct_snode sx in
            let t2 = find n2 m2 in
            (* (if !debug then 
            begin
                Format.printf "\n<=== Prop else ===> %s\n" (loc e2);
                pr_value Format.std_formatter t2;
                Format.printf "\n<<~~~~>> %s\n" l;
                pr_value Format.std_formatter (find n m2);
                Format.printf "\n";
            end
            ); *)
            let t2', t'' = 
                (* if optmization m2 n2 find && optmization m2 n find then t1, (find n m2) else *)
                (* let t2, t = alpha_rename_Vs t2 t in *)
                prop t2 t in
            (* (if !debug then
            begin
                Format.printf "\nRES for prop:\n";
                pr_value Format.std_formatter t2';
                Format.printf "\n<<~~~~>>\n";
                pr_value Format.std_formatter t'';
                Format.printf "\n";
            end
               );  *)
            (* (if (loc e1) = "63" then 
            begin
                Format.printf "\n<=== join then else ===> %s\n" (loc e1);
                pr_value Format.std_formatter t';
                Format.printf "\n<<~~~~>> %s\n" (loc e2);
                pr_value Format.std_formatter t'';
                Format.printf "\n";
            end
            ); *)
            (* let t'', t' = alpha_rename_Vs t'' t' in *)
            let t_n' = join_V (stren_V t' ae) (stren_V t'' ae) in
            (* (if (loc e1) = "63" then
            begin
                Format.printf "\nRES for join then else:\n";
                pr_value Format.std_formatter t_n';
                Format.printf "\n";
            end
               );  *)
            let res_m = m2 |> update n1 t1' |> update n2 t2' |> update n t_n' in
            res_m
        end
    | Rec (f_opt, (x, lx), e1, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Func ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let t0 = find n m in
        let t =
          match t0 with
          | Bot ->
              let te = let z = fresh_z () in init_T (z,z) in
              Table te
          | _ -> t0
        in
        let cs' = cs in
        let is_rec' = Opt.exist f_opt || is_rec in
        step_func (fun cs (tl, tr) m' -> 
            if tl = Bot then m' |> update n t
            else if tr = Top then top_M m' else
            begin
              let _, var = cs in
              let f_nf_opt =
                Opt.map (fun (f, lf) -> f, (construct_vnode env lf cs, true)) f_opt
              in
              let nx = construct_vnode env lx cs in
              let env' = env |> VarMap.add x (nx, false) in
              let nx = construct_snode x nx in
              let env1 =
                env' |>
                (Opt.map (uncurry VarMap.add) f_nf_opt |>
                Opt.get_or_else (fun env -> env))
              in
              let n1 = construct_enode env1 (loc e1) |> construct_snode x in
              let tx = find nx m in
              let ae' = if (x <> "_" && is_Relation tx) || is_List tx then 
                if only_shape_V tx then ae else (arrow_V x ae tx) else ae in
              let t1 = if x = "_" then find n1 m else replace_V (find n1 m) x var in
              let prop_t = Table (construct_table cs (tx, t1)) in
              (* (if !debug then
                 begin
                 Format.printf "\n<=== Prop lamb ===> %s %s\n" lx (loc e1);
                 pr_value Format.std_formatter prop_t;
                 Format.printf "\n<<~~~~>> %s\n" l;
                 pr_value Format.std_formatter t;
                 Format.printf "\n";
                 end
                 ); *)
              let px_t, t1 = prop_scope env1 env' x m prop_t t in
              (* (if !debug then
                 begin
                 Format.printf "\nRES for prop:\n";
                 pr_value Format.std_formatter px_t;
                 Format.printf "\n<<~~~~>>\n";
                 pr_value Format.std_formatter t1;
                 Format.printf "\n";
                 end
                 ); *)
              let nf_t2_tf'_opt =
                Opt.map (fun (_, (nf, bf)) ->
                  let envf, lf, fcs = get_vnode nf in
                  let nf = construct_snode x nf in
                  let tf = find nf m in
                  (* (if true then
                        begin
                            Format.printf "\n<=== Prop um ===> %s\n" l;
                            pr_value Format.std_formatter t;
                            Format.printf "\n<<~~~~>> %s\n" lf;
                            pr_value Format.std_formatter tf;
                            Format.printf "\n";
                        end
                    ); *)
                  let t2, tf' = prop_scope env' envf x m t tf in
                  (* let t2, tf' = prop t tf in *)
                  (* (if true then
                     begin
                     Format.printf "\nRES for prop:\n";
                     pr_value Format.std_formatter t2;
                     Format.printf "\n<<~~~~>>\n";
                     pr_value Format.std_formatter tf';
                     Format.printf "\n";
                     end
                     ); *)
                  nf, t2, tf') f_nf_opt
              in
              let tx', t1' = io_T cs px_t in
              let m1 = m |> update nx tx' |> update n1 (if x = "_" then t1' else replace_V t1' var x) |>
              (Opt.map (fun (nf, t2, tf') -> fun m' -> m' |> update nf tf' |> update n (join_V t1 t2))
                 nf_t2_tf'_opt |> Opt.get_or_else (update n t1)) in
              let cs = if is_rec' && x = "_" then cs' else cs in
              let m1' = step e1 env1 x cs ae' assertion is_rec' m1 in
              join_M m1' m'
            end
        ) t (m |> update n t |> Hashtbl.copy)
    | TupleLst (tlst, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Tuple ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let t = find n m in
        if List.length tlst = 0 then
            let t' = let ct = init_V_c UnitLit in
            join_V t ct in
            m |> update n t'
        else
            let tp, m' = List.fold_right (fun e (t, m) -> 
                let m' = step e env sx cs ae assertion is_rec m in
                let ne = construct_enode env (loc e) |> construct_snode sx in
                let te = find ne m' in
                let t' = add_tuple_item_V t te in
                t', m'
            ) tlst (Tuple [], m) in
            let _, t' = prop tp t in
            m' |> update n t'
    | PatMat (e, patlst, l) ->
        (* (if !debug then
        begin
            Format.printf "\n<=== Pattern Match ===>\n";
            pr_exp true Format.std_formatter term;
            Format.printf "\n";
        end
        ); *)
        let ne = construct_enode env (loc e) |> construct_snode sx in
        (* let ex = get_var_name e in *)
        let m' = step e env sx cs ae assertion is_rec m in
        let te = find ne m' in
        if te = Bot || only_shape_V te then m' else
        let m'' = List.fold_left (fun m (Case (e1, e2)) -> 
            (* (if !debug then
            begin
                Format.printf "\n<=== Pattern ===>\n";
                pr_exp true Format.std_formatter e1;
                Format.printf "\n";
                pr_exp true Format.std_formatter e2;
                Format.printf "\n";
            end
            ); *)
            match e1 with
            | Const (c, l') ->
                let m1 = step e1 env sx cs ae assertion is_rec m in
                let n1 = construct_enode env (loc e1) |> construct_snode sx in
                let te, t1 = find ne m1, find n1 m1 in
                let te, t1 = alpha_rename_Vs te t1 in
                if leq_V te t1 then m' else
                let t1 = if is_List te then
                    item_shape_V te t1
                    else t1 in
                (* (if true then
                    begin
                        Format.printf "\n %s\n" (loc e1);
                        pr_value Format.std_formatter t1;
                        Format.printf "\n<<~~~~>> %s\n" (loc e);
                        pr_value Format.std_formatter te;
                        Format.printf "\n";
                        pr_value Format.std_formatter (join_V t1 te);
                        Format.printf "\n";
                    end
                ); *)
                let m1 = m1 |> update n1 t1 in
                let b =
                    sat_leq_V t1 te 
                in
                let n1 = construct_enode env (loc e1) |> construct_snode sx in
                let n2 = construct_enode env (loc e2) |> construct_snode sx in
                (* (if true then
                begin
                    Format.printf "\npattern ae: %b\n" b;
                    pr_value Format.std_formatter ae;
                    Format.printf "\n";
                end
                ); *)
                let ae' = if not b then bot_relation_V Int else 
                    arrow_V (loc e) ae (find n1 m1) in
                (* (if true then
                begin
                    Format.printf "\nRES for ae:\n";
                    pr_value Format.std_formatter ae';
                    Format.printf "\n";
                end
                );  *)
                let m2 = step e2 env sx cs ae' assertion is_rec m1 in
                if not b then 
                    let t, t2 = find n m2, find n2 m2 in
                    let t2', t' = prop t2 t in
                    m2 |> update n1 t1 |> update n2 t2' |> update n t'
                else
                let te, t, t1, t2 = find ne m2, find n m2, find n1 m2, find n2 m2 in
                let t1', te' = let te, t1 = alpha_rename_Vs te t1 in 
                    t1, join_V t1 te in
                let t2', t' = prop t2 t in
                m2 |> update ne te' |> update n1 t1' 
                |> update n2 t2' |> update n t'
            | Var (x, l') ->
                let n1 = construct_vnode env l' (sx,sx) in
                let env1 = env |> VarMap.add x (n1, false) in
                let n1 = construct_snode x n1 in
                let t1 = find n1 m in let te = find ne m in
                let _, t1' = prop te t1 in
                let m1 = m |> update n1 t1' in
                let m2 = step e2 env1 sx cs ae assertion is_rec m1 in
                let n2 = construct_enode env1 (loc e2) |> construct_snode sx in
                let t = find n m2 in let t2 = find n2 m2 in
                let t2', t' = prop t2 t in
                m2 |> update n2 t2' |> update n t'
              | BinOp (Cons, el, er, l') ->
                  (* (if true then
                     begin
                     Format.printf "\n<=== Prop then ===> %s\n" (loc e1);
                     pr_value Format.std_formatter t1;
                     Format.printf "\n<<~~~~>> %s\n" l;
                     pr_value Format.std_formatter (find n m1);
                     Format.printf "\n";
                     end
                     ); *)
                  let ml, envl = list_var_item el sx cs m env ne true 0 in
                  (* (if !debug then
                     begin
                     Format.printf "\n<=== Pattern binop ===>\n";
                     pr_exp true Format.std_formatter er;
                     Format.printf "\n";
                     end
                     ); *)
                  let mr, envr = list_var_item er sx cs ml envl ne false 1 in
                  let nr = construct_enode envr (loc er) |> construct_snode sx in
                  let n2 = construct_enode envr (loc e2) |> construct_snode sx in
                  let n1 = construct_enode envr l' |> construct_snode sx in
                  let m1 = step e1 envr sx cs ae assertion is_rec mr in
                  let te, t1 = find ne m1, find n1 m1 in
                  let te, t1 = alpha_rename_Vs te t1 in
                  let ae' =
                    let ae = arrow_V (loc e) ae t1 in 
                    arrow_V (loc er) ae (find nr m1)
                  in
                  let m2 = step e2 envr sx cs ae' assertion is_rec m1 in
                  let te, t, t1, t2 = find ne m2, find n m2, find n1 m2, find n2 m2 in
                  let t1', te' =
                    let te, t1 = alpha_rename_Vs te t1 in 
                    prop t1 te
                  in
                  let t2', t' = prop t2 t in
                  m2 |> update ne te' |> update n1 t1'
                |> update n2 t2' |> update n t'
            | TupleLst (termlst, l') ->
                let n1 = construct_enode env l' |> construct_snode sx in
                let t1 = 
                    let raw_t1 = find n1 m in
                    if is_tuple_V raw_t1 then raw_t1
                    else 
                        let u' = List.init (List.length termlst) (fun _ ->
                    Bot) in Tuple u' in
                let te = find ne m in
                let te, t1 = alpha_rename_Vs te t1 in
                let tlst = get_tuple_list_V t1 in
                let tllst = te |> get_tuple_list_V in
                let env', m', tlst', tllst' = List.fold_left2 (fun (env, m, li, llst) e (ti, tlsti) -> 
                    match e with
                    | Var (x, l') -> 
                        let nx = construct_vnode env l' cs in
                        let env1 = env |> VarMap.add x (nx, false) in
                        let nx = construct_snode sx nx in
                        let tx = find nx m in
                        let ti', tx' = prop ti tx in
                        let tlsti', ti'' = prop tlsti ti in
                        let m' = m |> update nx tx' in
                        env1, m', (join_V ti' ti'') :: li, tlsti' :: llst
                    | _ -> raise (Invalid_argument "Tuple only for variables now")
                ) (env, m, [], []) termlst (zip_list tlst tllst) in
                let tlst', tllst' = List.rev tlst', List.rev tllst' in
                let t1', te' = Tuple tlst', Tuple tllst' in
                let _, t1' = prop t1' t1 in
                let te', _ = prop te te' in
                let m1 = m |> update n1 t1' |> update ne te' in
                let m2 = step e2 env' sx cs ae assertion is_rec m1 in
                let n2 = construct_enode env' (loc e2) |> construct_snode sx in
                let t = find n m2 in let t2 = find n2 m2 in
                let t2', t' = prop t2 t in
                m2 |> update n2 t2' |> update n t'
            | _ -> raise (Invalid_argument "Pattern should only be either constant, variable, or list cons")
        ) (update ne te m' |> Hashtbl.copy) patlst in
        m''

let step x1 x2 x3 x4 x5 x6 x7 = measure_call "step" (step x1 x2 x3 x4 x5 x6 x7)
          
(** Widening **)
let widening k (m1:exec_map_t) (m2:exec_map_t): exec_map_t =
  (*if k < 1 then wid_M m1 m2 else join_M m1 m2*)
  if k > !delay_wid then wid_M m1 m2 else join_M m1 m2

let widening k m = measure_call "widening" (widening k m)

    
(** Narrowing **)
let narrowing (m1:exec_map_t) (m2:exec_map_t): exec_map_t =
  meet_M m1 m2 

(** Fixpoint loop *)
let rec fix env e (k: int) (m:exec_map_t) (assertion:bool): string * exec_map_t =
  (if !out_put_level = 0 then
    begin
        Format.printf "%s step %d\n" !process k;
        print_exec_map m;
    end);
  (* if k > 40 then exit 0 else *)
  let ae = VarMap.fold (fun var (n, b) ae ->
    let n = construct_snode "" n in
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    let t = find n m in
    if is_Relation t then
    arrow_V var ae t else ae
    ) env (Relation (top_R Plus)) in
  let m_t = Hashtbl.copy m in
  let m' = step e env "" ("","") ae assertion false m_t in
  if k < 0 then if k = -1 then "", m' else fix env e (k+1) m' assertion else
  (* if k > 2 then Hashtbl.reset !pre_m;
  pre_m := m; *)
  (* Format.printf "\nFinish step %d\n" k;
  flush stdout; *)
  let m'' = if !process = "Wid" then widening k m m' else narrowing m m' in
  let comp = if !process = "Wid" then leq_M m'' m else leq_M m m'' in
  if comp then
      begin
      if assertion || !narrow then 
        let s = AssertionPosMap.fold (fun pos (i, j) s ->
            if i = j then try print_loc pos with 
            Input_Assert_failure s -> s else s) !sens "The input program is safe\n" in
        s, m 
      else 
        begin
        try fix env e k m true (* Final step to check assertions *)
        with Input_Assert_failure s -> s, m 
        end
      end
  else (fix env e (k+1) m'' assertion) (*Hashtbl.reset m; *)

      
(** Semantic function *)
let s e =
    (* (if !debug then
    begin
        Format.printf "%% Pre vals: %%\n";
        pr_pre_def_vars Format.std_formatter;
        Format.printf "\n\n";
    end); *)
    (* exit 0; *)
    (* let rec print_list = function 
    [] -> ()
    | e::l -> print_int e ; print_string " " ; print_list l in
    ThresholdsSetType.elements !thresholdsSet |> print_list; *)
    (* AbstractDomain.AbstractValue.licons_ref := AbstractDomain.AbstractValue.licons_earray [|"cur_v"|]; *)
  let envt, m0' = pref_M env0 (Hashtbl.copy m0) in
  let fv_e = fv e in
  let envt =
    VarMap.filter
      (fun x (n, _) ->
        if StringSet.mem x fv_e then true
        else
          let n = construct_snode "" n in
          (Hashtbl.remove m0' n; false)) envt
  in
  (*thresholdsSet := !thresholdsSet |> ThresholdsSetType.add 0 
  |> ThresholdsSetType.add 2 |> ThresholdsSetType.add 4 |> ThresholdsSetType.add (-1) |> ThresholdsSetType.add (-2);*)
  (* pre_m := m0'; *)
    let check_str, m =
      let s1, m1 = (fix envt e 0 m0' false) in
        if !narrow then
            begin
            process := "Nar";
            narrow := false;
            (*let _, m1 = fix envt e (-10) m1 false in (* step^10(fixw) <= fixw *)*)
            let m1 = m1 |> reset in
            let s2, m2 = fix envt e 0 m1 false in
            if eq_PM m0 m2 then s2, m2 
            else exit 0
            end
        else s1, m1
    in
    if !out_put_level = 1 then
        begin
        Format.printf "Final step \n";
        print_exec_map m;
        end;
    Format.printf "%s" check_str;
    m
