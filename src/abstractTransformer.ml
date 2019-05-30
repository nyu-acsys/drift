
open AbstractDomain
open Syntax
open SemanticsDomain

let prop v1 v2 = match v1, v2 with
    | Top, Bot | Table _, Top -> Top, Top
    | Table t, Bot -> v1, Table (SemanticsDomain.init_T (dx_T t))
    | Relation r1, Relation r2 -> Relation r1, Relation (SemanticsDomain.join_R r1 r2)
    | Table t1, Table t2 -> let t1', t2' = SemanticsDomain.alpha_rename t1 t2 in
        let (z, v1i, v1o) = t1' and (z, v2i, v2o) = t2' in
        let v1, v2 = let v2i', v1i' = prop v2i v1i and v1o', v2o' = prop (arrow_V "z" v1o v2i) (arrow_V "z" v2o v2i) in
        (v1i', SemanticsDomain.join_V v1o v1o'), (v2i', SemanticsDomain.join_V v2o v2o')
        in
        let t1'' = (z, fst v1, snd v1) and t2'' = (z, fst v2, snd v2) in
        Table t1'', Table t2''
    | Relation r, Table t -> let t' = let (z, vi, vo) = t in (z, prop r vi, prop r vo) in
        Relation r, Table t'
    | _, _ -> v1, v2

let iterUpdate m v l = NodeMap.map (fun x -> arrow_V (string_of_int l) x v) m

let iterMap m c = NodeMap.fold (fun n x v -> let EN (env, label) = n in
    SemanticsDomain.init_V_c c x (string_of_int label)) m Bot

let rec step term env m =
    let n = EN (env, loc term) in
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else Bot in
    match term with
    | Const (c, l) ->
        let v = find n m in (*M[env*l]*)
        if v = Bot then
        let cv = iterMap m c in
        let v' = join_V v cv in
        m |> NodeMap.add n v'
        else m
    | Var (x, l) ->
      let nx = VarMap.find x env in
      let vx = equal_V (string_of_int l) (find nx m) in
      let v = find n m in (*M[env*l]*)
      let tx', t' = prop vx v in
      m |> NodeMap.add nx tx' |> NodeMap.add n t'
    | App (e1, e2, l) ->
        let m1 = step e1 env m in
        let n1 = EN (env, loc e1) in
        let t1 = find n1 m1 in
        if t1 = Bot then m1
        else if not @@ SemanticsDomain.is_table v1 then
        (Printf.printf "Error at location %s: expected function, but found %s.\n"
        (string_of_int (loc e1)) (string_of_value v1);
        m1 |> NodeMap.add n Top)
        else
            let m2 = step e2 env m1 in
            let n2 = EN (env, loc e2) in
            let t2 = find n2 m2 in (*M[env*l2]*)
            (match t2 with
                | Bot -> m2
                | Top -> m2 |> NodeMap.add n Top
                | _ -> let t = find n m2 in
                let t_temp = (SemanticsDomain.dx_T t1), t2, t in
                let t1', t0 = prop t1 (Table t_temp) in
                let t2', t' = SemanticsDomain.io_T t0 in
                m2 |> NodeMap.add n1 t1' |> NodeMap.add n2 t2' |> NodeMap.add n t'
            )
    | BinOp (bop, e1, e2, l) ->
        let m1 = step e1 env m in
        let m2 = step e2 env m in
        let n1 = EN (env, loc e1) in
        let t1 = find n1 m1 in
        let n2 = EN (env, loc e2) in
        let t2 = find n2 m2 in
        m (*TODO: Add operation*)
    | Ite (e0, e1, e2, l) ->
        let m0 = step e0 env m in
        let n0 = EN (env, loc e0) in
        let t0 = find n0 m0 in
        if t0 = Bot then m0 else
        if not @@ SemanticsDomain.is_bool_V t0 then m0 |> NodeMap.add n Top else
        begin
            let t_true = meet_V (init_V_b true) t0 in
            let t_false = meet_V (init_V_b false) t0 in
            let m1 = step e1 env (iterUpdate m t_true (loc e0)) in
            let m2 = step e2 env (iterUpdate m t_false (loc e0)) in
            let n1 = EN (env, loc e1) in
            let t1 = find n1 m1 in
            let n2 = EN (env, loc e2) in
            let t2 = find n2 m2 in
            let t1', t' = prop t1 (find n m1) and t2', t'' = prop t2 (find n m2) in
            let m1' = m1 |> NodeMap.add n1 t1' |> NodeMap.add n t' and
            m2' = m2 |> NodeMap.add n2 t2' |> NodeMap.add n t'' in
            SemanticsDomain.join_M (SemanticsDomain.join_M m0 m1') m2'
        end
    | Rec (f_opt, x, lx, e1, l) -> m (*TODO: Add func implementation*)

(** Fixpoint loop *)
let rec fix e k env m =
  Printf.printf "step %d\n" k;
  print_exec_map stdout m;
  let m' = step e env m in
  if leq_M m' m then m
  else fix e (k + 1) env m'

(** Semantic function *)
let s e = fix e 0 VarMap.empty NodeMap.empty
