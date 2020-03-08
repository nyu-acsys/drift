open AbstractDomain
open Syntax
open SemanticDomain
open SemanticsDomain
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

let z_index = ref 0 (* first definition *)

let process = ref "Wid"

let env0, m0 = 
    array_M VarMap.empty (NodeMap.create 1234)

let pre_m = ref m0

let pre_def_func = [|"make"; "len"; "set"; "get"|]

let st = ref 0

let delay_threshold = 600

let incr_z () =
    z_index := !z_index + 1

let eq_PM (m1:exec_map_t) (m2:exec_map_t) =
    NodeMap.for_all (fun n v1 (*untie to node -> value*) ->
    NodeMap.find_opt n m2 |> Opt.map (
      fun v2 -> let SN (_, l) = n in
      if eq_V v1 v2 then true else 
      begin
        Format.printf "\nPre Def %s:\n" l;
        pr_value Format.std_formatter v1;
        Format.printf "\nNew %s\n" l;
        pr_value Format.std_formatter v2;
        Format.printf "\n";
        raise (Pre_Def_Change ("Predefined node changed at " ^ l))
      end
      )
    |> Opt.get_or_else (v1 = v1)) m1

let rec prop (v1: value_t) (v2: value_t): (value_t * value_t) = match v1, v2 with
    | Top, Bot | Table _, Top -> Top, Top
    | Unit _, Bot -> v1, v1
    | Table t, Bot -> let t' = init_T (dx_Ta t) in
        v1, Table (t')
    | Ary ary, Bot -> let vars, r = ary in
        let ary' = init_Ary vars in
        v1, Ary ary'
    | Relation r1, Relation r2 -> Relation r1, Relation (join_R r1 r2)
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
            let v2i', v1i' = 
                (*Optimization 1: If those two are the same, ignore the prop step*)
                if is_Bot_V v1i = false && eq_V v2i v1i then v2i, v1i else 
                prop v2i v1i 
            and v1o', v2o' = 
                if is_Bot_V v2o = false && eq_V v1ot v2o then v1ot, v2o else 
                prop (arrow_V z1 v1ot v2i) (arrow_V z1 v2o v2i) 
            in
            let v1o' =
                if is_Array v1i' && is_Array v2i' then
                    let l1 = get_len_var_V v1i' in
                    let l2 = get_len_var_V v2i' in
                    replace_V v1o' l2 l1
                else v1o'
            in 
            (v1i', join_V v1o v1o'), (v2i', join_V v2o v2o') 
        in
        let t1'' = (z1, fst p1, snd p1) and t2'' = (z2, fst p2, snd p2) in
        Table t1'', Table t2''
    | _, _ -> v1, join_V v1 v2

let get_env_array (env: env_t) = 
    let env_l = (VarMap.bindings env) in
    let rec helper ls ary = match ls with
        | [] -> ary
        | (x, n) :: env -> helper env (Array.append ary [|x|])
    in
    helper env_l [||]

(* let lc_env env1 env2 = 
    Array.fold_left (fun a id -> if Array.mem id a then a else Array.append a [|id|] ) env2 env1 *)

let prop_scope (env1: env_t) (env2: env_t) (v1: value_t) (v2: value_t): (value_t * value_t) = 
    let env1 = get_env_array env1 in
    let env2 = get_env_array env2 in
    (* let v1'',_  = prop v1 (proj_V v2 env1)in
    let _, v2'' = prop (proj_V v1 env2) v2 in *)
    let v1', v2' = prop v1 v2 in
    let v1'' = proj_V v1' env1 in
    let v2'' = proj_V v2' env2 in
    v1'', v2''

(* let rec nav v1 v2 = match v1, v2 with (*precise abs*)
    | Relation r1, Relation r2 -> if leq_R r1 r2 then Relation r1 else Relation r2
    | Table t1, Table t2 -> let t1', t2' = alpha_rename t1 t2 in
        let (z1, v1i, v1o) = t1' and (z2, v2i, v2o) = t2' (*precise*) in
        let v1i' = nav v1i v2i in
        let v1o' = nav (arrow_V z1 v1o v2i) (arrow_V z1 v2o v2i) in
        let t1'' = (z1, v1i', v1o') in
        Table t1''
    | _, _ -> v1 *)

(* let iterUpdate m v l = NodeMap.map (fun x -> arrow_V l x v) m *)

let getVars env = VarMap.fold (fun var n ary -> Array.append ary [|var|]) env [||]

(* let iterEnv_c env m c = VarMap.fold (fun var n a ->
    let v = NodeMap.find_opt n m |> Opt.get_or_else Top in
    let EN (env', l) = n in
    let lb = name_of_node l in
    c_V a v lb) env (init_V_c c)

let iterEnv_v env m v = VarMap.fold (fun var n a -> 
    let ai = NodeMap.find_opt n m |> Opt.get_or_else Top in
    arrow_V var a ai) env v *)

let optmization m n find =
    if !st <= delay_threshold then false else
    let t = find n m in
    let pre_t = find n !pre_m in
    opt_eq_V pre_t t

let rec step term (env: env_t) (m:exec_map_t) (ae: value_t) =
    let n = SN (true, loc term) in
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    match term with
    | Void l ->
        let t' = Unit () in
        m |> NodeMap.add n t'
    | Const (c, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Const ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        if optmization m n find then m else
        let t = find n m in (* M[env*l] *)
        let ct = init_V_c c in
        let t' = join_V t ct in
        m |> NodeMap.add n (stren_V t' ae) (* {v = c ^ aE}*)
    | Var (x, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Var ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let nx = VarMap.find x env in
        let EN (envx, lx) = nx in
        let nx = SN (true, lx) in
        let tx = let tx' = find nx m in
            if is_Relation tx' then equal_V tx' x (* M<E(x)>[v=E(x)] *) 
            else tx'
        in 
        let t = find n m in (* M[env*l] *)
        (if !debug then
        begin
            Format.printf "\n<=== Prop Var ===> %s\n" lx;
            pr_value Format.std_formatter tx;
            Format.printf "\n<<~~~~>> %s\n" l;
            pr_value Format.std_formatter t;
            Format.printf "\n";
        end
        );
        let raw_tx', t' =
            if optmization m n find && optmization m nx find then tx, t
            else if Array.mem lx pre_def_func then
                prop tx t
            else
                prop_scope envx env tx t
        in
        let tx' = forget_V x raw_tx' in
        (if !debug then
        begin
            Format.printf "\nRES for prop:\n";
            pr_value Format.std_formatter tx';
            Format.printf "\n<<~~~~>>\n";
            pr_value Format.std_formatter t';
            Format.printf "\n";
        end
        );
        m |> NodeMap.add nx tx' |> NodeMap.add n (stren_V t' ae) (* t' ^ ae *)
    | App (e1, e2, l) ->
        (if !debug then
        begin
        Format.printf "\nStart <=== App ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let m1 = step e1 env m ae in
        let n1 = SN (true, loc e1) in
        let t1 = find n1 m1 in
        if t1 = Bot then m1
        else if not @@ is_table t1 then
        (Format.printf "Error at location %s: expected function, but found %s.\n"
        (loc e1) (string_of_value t1);
        m1 |> NodeMap.add n Top)
        else
            let m2 = step e2 env m1 ae in
            let n2 = SN (true, loc e2) in
            let t2 = find n2 m2 in (* M[env*l2] *)
            (match t2 with
                | Bot -> m2
                | Top -> m2 |> NodeMap.add n Top
                | _ -> let t = find n m2 in
                let t_temp = Table ((dx_T t1), t2, t) in
                (if !debug then
                begin
                    Format.printf "\n<=== Prop APP ===> %s\n" (loc e1);
                    pr_value Format.std_formatter t1;
                    Format.printf "\n<<~~~~>> %s\n" l;
                    pr_value Format.std_formatter t_temp;
                    Format.printf "\n";
                end
                );
                let t1', t0 = 
                    if optmization m2 n1 find && optmization m2 n2 find && optmization m2 n find then t1, t_temp else
                    prop t1 t_temp
                in
                (if !debug then
                begin
                    Format.printf "\nRES for prop:\n";
                    pr_value Format.std_formatter t1';
                    Format.printf "\n<<~~~~>>\n";
                    pr_value Format.std_formatter t0;
                    Format.printf "\n";
                end
                );
                let t2', raw_t' = io_T t0 in
                let t' = getVars env |> proj_V raw_t' in
                m2 |> NodeMap.add n1 t1' |> NodeMap.add n2 t2' |> NodeMap.add n t'
            )
    | BinOp (bop, e1, e2, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Binop ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let m1 = step e1 env m ae in
        let m2 = step e2 env m1 ae in
        let n1 = SN (true, loc e1) in
        let t1 = find n1 m2 in
        let n2 = SN (true, loc e2) in
        let t2 = find n2 m2 in
        if t1 = Bot || t2 = Bot then m2 
        else
        begin
            (if not @@ is_Relation t1 then 
            (Format.printf "Error at location %s: expected value, but found %s.\n"
                (loc e1) (string_of_value t1))
            else (if not @@ is_Relation t2 then
            Format.printf "Error at location %s: expected value, but found %s.\n"
                (loc e2) (string_of_value t2))
            );
            let t = find n m2 in
            (* if optmization m2 n1 find && optmization m2 n2 find && optmization m2 n find then m else *)
            let td = Relation (top_R bop) in
            let raw_t = if bool_op bop then
            begin
                bool_op_V bop t1 t2
            end
            else
            begin
                (* {v:int | a(t) ^ v = n1 op n2 }[n1 <- t1, n2 <- t2] *)
                let node_1 = e1 |> loc |> name_of_node in
                let node_2 = e2 |> loc |> name_of_node in
                let t' = arrow_V node_1 td t1 in
                let t'' = arrow_V node_2 t' t2 in
                let t''' = op_V node_1 node_2 bop t'' in
                let temp_t = getVars env |> proj_V t''' in
                if !domain = "Box" then
                temp_t |> der_V e1 |> der_V e2  (*Solve remain constriant only for box*)
                else temp_t
            end
            in
            let _, re_t = if is_Relation raw_t then raw_t,raw_t else prop raw_t t in
            m2 |> NodeMap.add n re_t
        end
    | Ite (e0, e1, e2, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Ite ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let n0 = SN (true, loc e0) in
        let m0 = 
            if optmization m0 n0 find then m0 else 
            step e0 env m ae in
        let t0 = find n0 m0 in
        if t0 = Bot then m0 else
        if not @@ SemanticsDomain.is_bool_V t0 then m0 |> NodeMap.add n Top else
        begin
            let t_true = stren_V (extrac_bool_V t0 true) ae in (* Meet with ae*)
            let t_false = stren_V (extrac_bool_V t0 false) ae in
            let m0' = Hashtbl.copy m0 in
            let m1 = step e1 env m0 t_true in
            let m2 = step e2 env m0' t_false in
            let n1 = SN (true, loc e1) in
            let t1 = find n1 m1 in
            let n2 = SN (true, loc e2) in
            let t2 = find n2 m2 in
            ((if !debug then
            begin
                Format.printf "\n<=== Prop then ===> %s\n" (loc e1);
                pr_value Format.std_formatter t1;
                Format.printf "\n<<~~~~>> %s\n" l;
                pr_value Format.std_formatter (find n m1);
                Format.printf "\n";
            end
            );
            let t1', t' = 
                if optmization m1 n1 find && optmization m1 n find then t1, (find n m1) else 
                prop t1 (find n m1) in
            (if !debug then
            begin
                Format.printf "\nRES for prop:\n";
                pr_value Format.std_formatter t1';
                Format.printf "\n<<~~~~>>\n";
                pr_value Format.std_formatter t';
                Format.printf "\n";
            end
            ); 
            (if !debug then 
            begin
                Format.printf "\n<=== Prop else ===> %s\n" (loc e2);
                pr_value Format.std_formatter t2;
                Format.printf "\n<<~~~~>> %s\n" l;
                pr_value Format.std_formatter (find n m2);
                Format.printf "\n";
            end
            );
            let t2', t'' = 
                if optmization m2 n2 find && optmization m2 n find then t1, (find n m2) else
                prop t2 (find n m2) in
            (if !debug then
            begin
                Format.printf "\nRES for prop:\n";
                pr_value Format.std_formatter t2';
                Format.printf "\n<<~~~~>>\n";
                pr_value Format.std_formatter t'';
                Format.printf "\n";
            end
            ); 
            let m1' = m1 |> NodeMap.add n1 t1' |> NodeMap.add n (stren_V t' ae) and
            m2' = m2 |> NodeMap.add n2 t2' |> NodeMap.add n (stren_V t'' ae) in
            join_M (join_M m0 m1') m2')
        end
    | Rec (f_opt, x, lx, e1, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Func ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let t0 = find n m in
        let t = if t0 = Bot then 
            begin
                let te = init_T ("z"^(string_of_int !z_index)) in
                incr_z ();
                Table (te)
            end
            else t0 in
        let tl, tr = io_T t in
        if tl = Bot then m |> NodeMap.add n t
        else 
        if tr = Top then top_M m else
        begin
            let nx = EN (env, lx) in
            let f_nf_opt = Opt.map (fun (f, lf) -> f, EN (env, lf)) f_opt in
            let env1 =
                env |> VarMap.add x nx |>
                (Opt.map (uncurry VarMap.add) f_nf_opt |>
                 Opt.get_or_else (fun env -> env))
            in
            let n1 = SN (true, loc e1) in
            let nx = SN (true, lx) in
            let tx = find nx m in
            let ae' = if is_Relation tx && x <> "_" then (arrow_V x ae tx) else ae in
            let temp_t = if x = "_" then find n1 m else replace_V (find n1 m) x (dx_T t) in
            let prop_t = Table ((dx_T t), tx, temp_t) in
            (if !debug then
            begin
                Format.printf "\n<=== Prop lamb ===> %s %s\n" lx (loc e1);
                pr_value Format.std_formatter prop_t;
                Format.printf "\n<<~~~~>> %s\n" l;
                pr_value Format.std_formatter t;
                Format.printf "\n";
            end
            );
            let px_t, t1 = 
                if optmization m n find && optmization m nx find && optmization m n1 find then prop_t, t else
                prop_scope env1 env prop_t t
            in
            (if !debug then
            begin
                Format.printf "\nRES for prop:\n";
                pr_value Format.std_formatter px_t;
                Format.printf "\n<<~~~~>>\n";
                pr_value Format.std_formatter t1;
                Format.printf "\n";
            end
            );
            let nf_t2_tf'_opt =
                Opt.map (fun (_, nf) ->
                  let EN (envf,lf) = nf in
                  let nf = SN (true,lf) in
                  let tf = find nf m in
                  if optmization m n find && optmization m nf find then nf, t, tf else
                    begin
                    (if !debug then
                        begin
                            Format.printf "\n<=== Prop um ===> %s\n" l;
                            pr_value Format.std_formatter t;
                            Format.printf "\n<<~~~~>> %s\n" lf;
                            pr_value Format.std_formatter tf;
                            Format.printf "\n";
                        end
                    );
                    let t2, tf' = prop_scope env envf t tf in
                    (if !debug then
                        begin
                            Format.printf "\nRES for prop:\n";
                            pr_value Format.std_formatter t2;
                            Format.printf "\n<<~~~~>>\n";
                            pr_value Format.std_formatter tf';
                            Format.printf "\n";
                        end
                    );
                    nf, t2, tf'
                    end
                ) f_nf_opt
            in
            let tx', t1' = io_T px_t in
            let m1 = m |> NodeMap.add nx tx' |> NodeMap.add n1 (if x = "_" then t1' else replace_V t1' (dx_T t) x) |>
            (Opt.map (fun (nf, t2, tf') -> fun m' -> m' |> NodeMap.add nf tf' |> NodeMap.add n (join_V t1 t2))
            nf_t2_tf'_opt |> Opt.get_or_else (NodeMap.add n t1)) in
            step e1 env1 m1 ae'
        end 

(** Widening **)
let widening (m1:exec_map_t) (m2:exec_map_t): exec_map_t = let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    NodeMap.mapi (fun n t -> (*Delay wid*)
        if !st > delay_threshold then wid_V (find n m1) t else join_V t (find n m1)
    ) m2

(** Narrowing **)
let narrowing (m1:exec_map_t) (m2:exec_map_t): exec_map_t = let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    NodeMap.mapi (fun n t ->
        meet_V (find n m1) t
    ) m2

let env = ref env0

(** Fixpoint loop *)
let rec fix e (k: int) (m:exec_map_t): exec_map_t =
  st := k;
  (if not !integrat_test then
    begin
        Format.printf "%s step %d\n" !process k;
        print_exec_map m;
    end);
  let ae = Relation (top_R Plus) in
  let m_t = Hashtbl.copy m in
  let m' = step e !env m_t ae in
  let m'' = if !process = "Wid" then widening m m' else narrowing m m' in
  let pre_ch = eq_PM m0 m'' in
  if pre_ch then
  begin
    let comp = if !process = "Wid" then leq_M m'' m else leq_M m m'' in
    if comp then m
    else fix e (k+1) m''
  end
  else exit 0

(** Semantic function *)
let s e =
    (if !debug then
    begin
        Format.printf "%% Pre vals: %%\n";
        pr_pre_def_vars Format.std_formatter;
        Format.printf "\n\n";
    end);
    (* exit 0; *)
    let envt, m0' = pref_M !env (Hashtbl.copy m0) in
    env := envt;
    pre_m := m0';
    let m1 = (fix e 0 m0') in
    process := "Nar";
    let m = (fix e 0 m1) in
    (if !integrat_test then
        print_last_node m);
    m



