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
** TODO: True False
** Using v = 1 be true once inside vt and v = 1 be false inside vf
*)

let z_index = ref 0 (* first definition *)

let incr_z () =
    z_index := !z_index + 1

let rec prop v1 v2 = match v1, v2 with
    | Top, Bot | Table _, Top -> Top, Top
    | Table t, Bot -> let t' = init_T (dx_Ta t)  in
        v1, Table (t')
    | Relation r1, Relation r2 -> Relation r1, Relation (join_R r1 r2)
    | Table t1, Table t2 -> let t1', t2' = alpha_rename t1 t2 in
        let (z1, v1i, v1o) = t1' and (z2, v2i, v2o) = t2' in
        let p1, p2 = let v2i', v1i' = prop v2i v1i and v1o', v2o' = prop (arrow_V z1 v1o v2i) (arrow_V z1 v2o v2i) in
        (v1i', join_V v1o v1o'), (v2i', join_V v2o v2o') in
        let t1'' = (z1, fst p1, snd p1) and t2'' = (z2, fst p2, snd p2) in
        Table t1'', Table t2''
    | _, _ -> v1, join_V v1 v2

let iterUpdate m v l = NodeMap.map (fun x -> arrow_V l x v) m

let getVars env = VarMap.fold (fun var n ary -> Array.append ary [|var|]) env [||]

let iterEnv_c env m c = VarMap.fold (fun var n a ->
    let v = NodeMap.find_opt n m |> Opt.get_or_else Top in
    let EN (env', l) = n in
    let lb = name_of_node l in
    c_V a v lb) env (init_V_c c)

let iterEnv_v env m v = VarMap.fold (fun var n a -> 
    let ai = NodeMap.find_opt n m |> Opt.get_or_else Top in
    arrow_V var a ai) env v

let rec step term env m ae =
    let n = EN (env, loc term) in
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    match term with
    | Const (c, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Const ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let t = find n m in (* M[env*l] *)
        let ct = stren_V (init_V_c c) ae in (* {v = c ^ aE}*)
        let t' = join_V t ct in
        m |> NodeMap.add n t'
    | Var (x, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Var ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let nx = VarMap.find x env in
        let EN (env, lx) = nx in
        let tx = let tx' = find nx m in
            if is_table tx' then tx' else
            equal_V tx' x in (* M<E(x)>[v=E(x)] *)
        let t = find n m in (* M[env*l] *)
        let tx', t' = prop tx t in
        (if !debug then
        begin
            Format.printf "\n<=== Prop ===> %s\n" (string_of_int lx);
            pr_value Format.std_formatter tx;
            Format.printf "\n<<~~~~>> %s\n" (string_of_int l);
            pr_value Format.std_formatter t;
            Format.printf "\n";
            Format.printf "RES for prop:\n";
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
        Format.printf "\n<=== App ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let m1 = step e1 env m ae in
        let n1 = EN (env, loc e1) in
        let t1 = find n1 m1 in
        if t1 = Bot then m1
        else if not @@ is_table t1 then
        (Format.printf "Error at location %s: expected function, but found %s.\n"
        (string_of_int (loc e1)) (string_of_value t1);
        m1 |> NodeMap.add n Top)
        else
            let m2 = step e2 env m1 ae in
            let n2 = EN (env, loc e2) in
            let t2 = find n2 m2 in (* M[env*l2] *)
            (match t2 with
                | Bot -> m2
                | Top -> m2 |> NodeMap.add n Top
                | _ -> let t = find n m2 in
                let t_temp = Table ((dx_T t1), t2, t) in
                let t1', t0 = prop t1 t_temp in
                (if !debug then
                begin
                    Format.printf "\n<=== Prop ===> %s\n" (string_of_int (loc e1));
                    pr_value Format.std_formatter t1;
                    Format.printf "\n<<~~~~>> %s\n" (string_of_int l);
                    pr_value Format.std_formatter t_temp;
                    Format.printf "\n";
                    Format.printf "RES for prop:\n";
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
    | BinOp (bop, e1, e2, l) -> (*TODO: project all unrelated label, scope of label should only inside that node *)
        (if !debug then
        begin
        Format.printf "\n<=== Binop ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let m1 = step e1 env m ae in
        let m2 = step e2 env m1 ae in
        let n1 = EN (env, loc e1) in
        let t1 = find n1 m2 in
        let n2 = EN (env, loc e2) in
        let t2 = find n2 m2 in
        let t = find n m2 in
        let td = Relation (top_R bop) in
        (* {v:int | a(t) ^ v = n1 + n2 }[n1 <- t1, n2 <- t2] *)
        let node_1 = e1 |> loc |> name_of_node in
        let node_2 = e2 |> loc |> name_of_node in
        let t' = arrow_V node_1 td t1 in
        let t'' = arrow_V node_2 t' t2 in
        let t''' = op_V node_1 node_2 bop t'' in
        let temp_t = getVars env |> proj_V t''' in
        let raw_t = 
            if !domain = "Box" then
            temp_t |> der_V e1 |> der_V e2  (*Solve remain constriant only for box*)
            else temp_t
        in
        let _, re_t = prop raw_t t in
        (if is_table t1 then 
        (Format.printf "Error at location %s: expected value, but found %s.\n"
            (string_of_int (loc e1)) (string_of_value t1))
        else (if is_table t2 then
        Format.printf "Error at location %s: expected value, but found %s.\n"
            (string_of_int (loc e2)) (string_of_value t2))
        );
        m2 |> NodeMap.add n re_t
    | Ite (e0, e1, e2, l) ->
        (if !debug then
        begin
        Format.printf "\n<=== Ite ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
        end
        );
        let m0 = step e0 env m ae in
        let n0 = EN (env, loc e0) in
        let t0 = find n0 m0 in
        if t0 = Bot then m0 else
        if not @@ SemanticsDomain.is_bool_V t0 then m0 |> NodeMap.add n Top else
        begin
            let t_true = stren_V (extrac_bool_V t0 true) ae in (* Meet with ae*)
            let t_false = stren_V (extrac_bool_V t0 false) ae in
            let m1 = step e1 env m0 t_true in
            let m2 = step e2 env m0 t_false in
            let n1 = EN (env, loc e1) in
            let t1 = find n1 m1 in
            let n2 = EN (env, loc e2) in
            let t2 = find n2 m2 in
            let t1', t' = prop t1 (find n m1) and t2', t'' = prop t2 (find n m2) in
            let m1' = m1 |> NodeMap.add n1 t1' |> NodeMap.add n (stren_V t' t_true) and
            m2' = m2 |> NodeMap.add n2 t2' |> NodeMap.add n (stren_V t'' t_false) in
            join_M (join_M m0 m1') m2'
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
            let n1 = EN (env1, loc e1) in 
            let tx = find nx m in
            let ae' = if is_Relation tx then (arrow_V x ae tx) else ae in
            let temp_t = replace_V (find n1 m) x (dx_T t) in
            let prop_t = Table ((dx_T t), tx, temp_t) in
            let px_t, t1 = prop prop_t t in
            (if !debug then
            begin
                Format.printf "\n<=== Prop ===> %s\n" (string_of_int lx);
                pr_value Format.std_formatter prop_t;
                Format.printf "\n<<~~~~>> %s\n" (string_of_int l);
                pr_value Format.std_formatter t;
                Format.printf "\n";
                Format.printf "RES for prop:\n";
                pr_value Format.std_formatter px_t;
                Format.printf "\n<<~~~~>>\n";
                pr_value Format.std_formatter t1;
                Format.printf "\n";
            end
            );
            let nf_t2_tf'_opt =
                Opt.map (fun (_, nf) ->
                  let tf = find nf m in
                  let t2, tf' = prop t tf in
                  nf, t2, tf') f_nf_opt
            in
            let tx', t1' = io_T px_t in
            let m1 = m |> NodeMap.add nx tx' |> NodeMap.add n1 (replace_V t1' (dx_T t) x) |>
            (Opt.map (fun (nf, t2, tf') -> fun m' -> m' |> NodeMap.add nf tf' |> NodeMap.add n (join_V t1 t2))
            nf_t2_tf'_opt |> Opt.get_or_else (NodeMap.add n t1)) in
            step e1 env1 m1 ae'
        end 
        

(** Widening **)
let widening m1 m2 = let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    NodeMap.mapi (fun n a -> wid_V a (find n m1)) m2

(** Fixpoint loop *)
let rec fix e k env m =
  if true then
    begin
        Format.printf "step %d\n" k;
        print_exec_map m 
    end
  else exit 0;
  let m' = step e env m Top in
  let m'' = widening m m' in
  if leq_M m'' m then m
  else fix e (k + 1) env m''

(** Semantic function *)
let s e = fix e 0 VarMap.empty NodeMap.empty