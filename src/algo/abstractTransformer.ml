open AbstractDomain
open Syntax
open SemanticDomain
open SemanticsDomain
open SensitiveDomain
open SenSemantics
open TracePartDomain
open Printer
open Util
open Config
open AbstractEv
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

type stage_t =
  | Widening
  | Narrowing
  
(* module AssertionPosMap =  Map.Make(struct
  type t = Syntax.pos
  let compare = compare
  end)

type asst_map_t = (int * int) AssertionPosMap.t
let sens : asst_map_t ref = ref AssertionPosMap.empty
*)

module AsstsMap = Map.Make (struct type t = loc let compare = compare end) 
type regular_asst_t = (node_s_t * value_tt) list
type ev_asst_t = (node_s_t * env_t * trace_t) list
let regassts: regular_asst_t AsstsMap.t ref = ref AsstsMap.empty
let evassts: ev_asst_t AsstsMap.t ref = ref AsstsMap.empty
let add_reg_asst l ae n = 
  (if !debug then 
     begin 
       Format.fprintf Format.std_formatter "\nassert_regular added: @[<v>@[l:%s@]@,@[ae:%a@]@,@[n:%a@]@]"
         l pr_value ae pr_node n
     end);
  regassts := AsstsMap.update l 
                (function | None -> Some [(n, ae)] 
                          | Some assts -> if not @@ List.mem (n, ae) assts then Some ((n, ae)::assts)
                                         else Some assts
                ) !regassts
let add_ev_asst l (n, env, trace) = 
  (if !debug then 
     begin 
       Format.fprintf Format.std_formatter "\nassert_ev added: @[<v>@[l:%s@]@,@[n:%a@]@]"
         l pr_node n
     end);
  evassts := AsstsMap.update l 
               (function | None -> Some [(n, env, trace)] 
                         | Some assts -> if not @@ List.mem (n, env, trace) assts then 
                                          Some ((n, env, trace)::assts)
                                        else Some assts
               ) !evassts

let env0, m0 = 
    let enva, ma = array_M VarMap.empty (NodeMap.create 500) in
    let envb, mb = list_M enva ma in
    envb, mb

(* Create a fresh variable name *)
let fresh_z= fresh_func "z"

(* let arrow_V x1 x2 = measure_call "arrow_V" (arrow_V x1 x2) *)
(* let forget_V x = measure_call "forget_V" (forget_V x) *)
(* let stren_V x = measure_call "stren_V" (stren_V x) *)
(* let join_V e = measure_call "join_V" (join_V e)
let meet_V e = measure_call "meet_V" (meet_V e) *)
let equal_V e = measure_call "equal_V" (equal_V e)
let proj_V e = measure_call "proj_V" (proj_V e)
let sat_equal_V e = measure_call "sat_equal_V" (sat_equal_V e)
let arrow_VE x1 x2 = measure_call "arrow_VE" (arrow_VE x1 x2)
let arrow_VE_Eff x1 x2 = measure_call "arrow_VE_Eff" (arrow_VE_Eff x1 x2)
let forget_VE x = measure_call "forget_VE" (forget_VE x)
let stren_VE x = measure_call "stren_VE" (stren_VE x)
let stren_VE_Eff x = measure_call "stren_VE_Eff" (stren_VE_Eff x)
let join_VE e = measure_call "join_VE" (join_VE e)
let join_VE_Eff e = measure_call "join_VE_Eff" (join_VE_Eff e)
let meet_VE e = measure_call "meet_VE" (meet_VE e)
let equal_VE e = measure_call "equal_VE" (equal_VE e)
let proj_VE e = measure_call "proj_VE" (proj_VE e)
let sat_equal_VE e = measure_call "sat_equal_VE" (sat_equal_VE e)
let ev penv e = measure_call "ev" (AbstractEv.ev penv e)
let leq_M m1 = measure_call "leq_M" (leq_M m1)
let hashtblcopy = measure_call "Hashtbl.copy" (Hashtbl.copy)
let eff0 = measure_call "eff0" (eff0)

let get_effmap = function Effect ec -> ec | EffBot | EffTop -> StateMap.empty

let rec prop (ve1: value_te) (ve2: value_te): (value_te * value_te) = 
  (* (if !debug then
    let pr = Format.fprintf Format.std_formatter in
    pr "@.LINE 87, @,ve1:@[%a@]@, @,ve2:@[%a@]@."
      pr_value_and_eff ve1 pr_value_and_eff ve2); *)
    match ve1, ve2 with
    | TEBot, _ -> TEBot, ve2 
    | TypeAndEff _, TETop -> TETop, TETop
    | TypeAndEff (v,e), TEBot -> 
       let (v1, v2) = prop_v v Bot in 
       (TypeAndEff (v1, e)), (TypeAndEff (v2, e))
    | TypeAndEff (v1, e1), TypeAndEff(v2, e2) -> 
       if v2 = Top then (TETop, TETop)
       else
         let v1', v2' = prop_v v1 v2 in
         let e1', e2' = prop_eff e1 e2 in 
         (TypeAndEff (v1', e1'), TypeAndEff (v2', e2'))
    | TETop, _ -> TETop, TETop
and prop_eff eff1 eff2 = match eff1, eff2 with
    | EffTop, _ -> EffTop, EffTop
    | _, EffTop -> eff1, EffTop
    | _, EffBot -> eff1, eff1 
    | EffBot, _ -> EffBot, eff2
    | Effect _, Effect _ -> eff1, (join_Eff eff1 eff2)
and prop_v (v1: value_tt) (v2: value_tt): (value_tt * value_tt) = match v1, v2 with
    | Top, Bot | Table _, Top -> Top, Top
    | Table t, Bot ->
        let t' = init_T (dx_T (TypeAndEff (v1, empty_eff))) (get_env_T t) in
        v1, Table t'
    | Relation r1, Relation r2 ->
        if leq_R r1 r2 then v1, v2 else
        Relation r1, Relation (join_R r1 r2)
    | Table t1, Table t2 ->
       let prop_table_entry cs (ve1i, ve1o) (ve2i, ve2o) =
         let destruct_VE = function 
           | TEBot -> Bot, EffBot
           | TypeAndEff (v, e) -> v, e
           | TETop -> Top, EffTop 
         in
         let z = cs in
         let z = get_trace_data z in
         let v1i, _ = destruct_VE ve1i in
         let v2i, _ = destruct_VE ve2i in
         let ve1ot, ve2ip = 
           if (is_Array v1i && is_Array v2i) || (is_List v1i && is_List v2i) then
             let l1 = get_len_var_V v1i in
             let e1 = get_item_var_V v1i in
             let l2 = get_len_var_V v2i in
             let e2 = get_item_var_V v2i in
             let ve = replace_VE ve1o l1 l2 in
             replace_VE ve e1 e2, (forget_VE l1 ve2i |> forget_VE e1)
           else ve1o, ve2i
         in
         let ve1i', ve2i', ve1o', ve2o' = 
           let opt_i = false 
                       (*Optimization 1: If those two are the same, ignore the prop step*)
                       || (ve1i <> TEBot && ve2i <> TEBot && eq_VE ve2i ve1i) in
           let ve2i', ve1i' = 
             if opt_i then ve2i, ve1i else 
               prop ve2i ve1i 
           in
           let opt_o = false
                       (*Optimization 2: If those two are the same, ignore the prop step*)
                       || (ve2o <> TEBot && ve1o <> TEBot && eq_VE ve1ot ve2o)
           in
           let ve1o', ve2o' =
              let v2ip, e2ip = match ve2ip with (*TODO: why not use ve2i'?*)
                | TEBot -> Bot, EffBot
                | TypeAndEff (v, e) -> v, e
                | TETop -> Top, EffTop 
              in
              (* (if !debug then
                let pr = Format.fprintf Format.std_formatter in
                pr "@.Prop Table Outputs (Pre):@[<2>@[<v>@[ve1ot: @[%a@]@],@,@[ve2ip: @[%a@]@], \
                    @,@[ve2o: @[%a@]@]@]@]"
                  pr_value_and_eff ve1ot pr_value_and_eff ve2ip pr_value_and_eff ve2o); *)
              
              let ve1o', ve2o' =
                if only_shape_V v2ip then
                  TEBot, TEBot
                else
                  if opt_o then ve1ot, ve2o else
                    let vals = 
                      if io_effects () then get_effmap e2ip |> get_input_eff_relations z
                      else []
                    in
                    let ve1 = 
                      (* if not !quick_prop then *)
                        arrow_VE z ve1ot v2ip
                        |> fun ve -> (
                          if io_effects () then
                            let l = List.map (fun v -> stren_VE_Eff ve (Relation v)) vals in
                            List.fold_left (fun ve1 ve2 -> join_VE_Eff ve1 ve2) (List.hd l) (List.tl l)
                          else ve)
                      (* else ve1ot *)
                    in
                    let ve2 =
                      (* if not !quick_prop then *)
                        arrow_VE z ve2o v2ip
                        |> fun ve -> (
                          if io_effects () then
                            let l = List.map (fun v -> stren_VE_Eff ve (Relation v)) vals in
                            List.fold_left (fun ve1 ve2 -> join_VE_Eff ve1 ve2) (List.hd l) (List.tl l)
                          else ve)
                      (* else ve2o *)
                    in
                    if ve_have_effects () && extract_eff ve1 |> is_bot_Eff && (extract_v ve1 |> only_shape_V |> not)
                    then TEBot, TEBot else prop ve1 ve2
              in
                
              let ve2o' = 
                if extract_v ve2o' |> only_shape_V |> not && extract_eff ve2o' |> is_bot_Eff
                  then temap (id, (fun _ -> arrow_EffV z e2ip v2ip)) ve2o'
                  else ve2o'
              in

              (* let ve2o' = match ve2o with 
                | TEBot -> temap (id, (fun _ -> arrow_EffV z e2ip v2ip)) ve2o'
                | _ -> ve2o' 
              in *)
              (* (if !debug then
                let pr = Format.fprintf Format.std_formatter in
                pr "@.Prop Table Outputs (Post):@[<2>@[<v>@[ve1o': @[%a@]@],@,@[ve2o': @[%a@]@]@]@]"
                  pr_value_and_eff ve1o' pr_value_and_eff ve2o'); *)

              ve1o', ve2o'
           in
           let v1i', _ = destruct_VE ve1i' in
           let v2i', _ = destruct_VE ve2i' in
           let ve1o' =
             if (is_Array v1i' && is_Array v2i') || (is_List v1i' && is_List v2i') then
                let l1 = get_len_var_V v1i' in
                let e1 = get_item_var_V v1i' in
                let l2 = get_len_var_V v2i' in
                let e2 = get_item_var_V v2i' in
                let ve = replace_VE ve1o' l2 l1 in
                replace_VE ve e2 e1
             else ve1o'
           in
           (* if true then
              begin
              (if true then
              begin
              Format.printf "\n<=== prop o ===>%s\n" z;
              pr_value Format.std_formatter v1ot;
              Format.printf "\n";
              Format.printf "\n<--- v2i --->\n";
              pr_value Format.std_formatter v2i;
              Format.printf "\n";
              pr_value Format.std_formatter v2ip;
              Format.printf "\n";
              pr_value Format.std_formatter (arrow_V z v1ot v2ip);
              Format.printf "\n<<~~~~>>\n";
              pr_value Format.std_formatter (arrow_V z v2o v2ip);
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
           let ve1o', ve2o' = 
              if opt_o then ve1o', ve2o' else (join_VE ve1o ve1o', join_VE ve2o ve2o')
           in
           (* (if !debug then
            let pr = Format.fprintf Format.std_formatter in
            pr "@.Prop Table Outputs (PostPost):@[<2>@[<v>,@,@[ve1i': @[%a@]@]@]@],@,@[ve2i': @[%a@]@]@]@]@[ve1o': @[%a@]@],@,@[ve2o': @[%a@]@]@]@]"
            pr_value_and_eff ve1i' pr_value_and_eff ve2i' pr_value_and_eff ve1o' pr_value_and_eff ve2o'); *)
           ve1i', ve2i', ve1o', ve2o'
         in
         (ve1i', ve1o'), (ve2i', ve2o') 
       in
       let t1', t2' =
         prop_table prop_table_entry alpha_rename_VE join_R t1 t2
       in
      (* (if !debug then
      let pr = Format.fprintf Format.std_formatter in
      pr "@.Prop Table Outputs (Final):@[<2>@[<v>,@,@[ve1i': @[%a@]@]@]@],@,@[ve2i': @[%a@]@]@]@]"
      pr_value (Table t1') pr_value (Table t2')); *)
       Table t1', Table t2'
    | Ary ary1, Ary ary2 -> let ary1', ary2' = alpha_rename_Arys ary1 ary2 in
                           let ary' = (join_Ary ary1' ary2') in
                           let _, ary2'' = alpha_rename_Arys ary2 ary'  in
                           (* let _ = alpha_rename_Arys ary1 ary' in *)
                           Ary ary1, Ary ary2''
    | Lst lst1, Lst lst2 ->
       (* (if true then
          begin
          Format.printf "\n<=== prop list ===>\n";
          pr_value Format.std_formatter (Lst lst1);
          Format.printf "\n<<~~~~>>\n";
          pr_value Format.std_formatter (Lst lst2);
          Format.printf "\n";
          end
          ); *)
       let lst1', lst2' = alpha_rename_Lsts lst1 lst2 in
        (* (if true then
           begin
           Format.printf "\n<=== after rename list ===>\n";
           pr_value Format.std_formatter (Lst lst1');
           Format.printf "\n<<~~~~>>\n";
           pr_value Format.std_formatter (Lst lst2');
           Format.printf "\n";
           end
           ); *)
        let lst1'', lst2'' = prop_Lst prop lst1' lst2' in
        (* (if true then
           begin
           Format.printf "\n<=== res ===>\n";
           pr_value Format.std_formatter (Lst lst1'');
           Format.printf "\n<<~~~~>>\n";
           pr_value Format.std_formatter (Lst lst2'');
           Format.printf "\n";
           end
           ); *)
        let _, lst2'' = alpha_rename_Lsts lst2 lst2'' in
        Lst lst1'', Lst lst2''
    | Ary ary, Bot -> let vars, _ = ary in
                     let ary' = init_Ary vars in
                     prop_v v1 (Ary ary')
    | Lst lst, Bot -> let vars, _ = lst in
                     let lst' = init_Lst vars in
                     prop_v v1 (Lst lst')
    | Tuple u, Bot ->  let u' = List.init (List.length u) (fun _ ->
                                   TEBot) in
                      prop_v v1 (Tuple u')
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

let multiprop_io l_cs (l_ve1i, ve1o) (l_ve2i, ve2o) =
  let pad_lvei l_vei =
    if List.length l_cs < List.length l_vei then failwith "input1 list too big"
    else if List.length l_cs > List.length l_vei then
      begin print_endline "huh";
      let padding = List.init (List.length l_vei - List.length l_cs) (fun _ -> TEBot) in
      l_vei @ padding end
    else l_vei  
  in
  let l_ve1i = pad_lvei l_ve1i in
  let l_ve2i = pad_lvei l_ve2i in
  let l_z = List.map get_trace_data l_cs in
  let l_ve1i', l_ve2i', ve1o', ve2o' = 
    let l_ve2i', l_ve1i' =
      List.map (fun (ve1i, ve2i) ->
        let opt_i = false
                    || (ve1i <> TEBot && ve2i <> TEBot && eq_VE ve2i ve1i) in
        if opt_i then ve2i, ve1i else 
          prop ve2i ve1i 
      ) (List.combine l_ve1i l_ve2i)
      |> List.split
    in
    let opt_o = false
                || (ve2o <> TEBot && ve1o <> TEBot && eq_VE ve1o ve2o)
    in
    let ve1o', ve2o' =
      let l_v2i, l_e2i =
        List.map (
          function
          | TEBot -> Bot, EffBot
          | TypeAndEff (v, e) -> v, e
          | TETop -> Top, EffTop
        ) l_ve2i |> List.split
      in
      
      let ve1o', ve2o' =
        if (List.fold_left (fun res v2i -> if res then res else only_shape_V v2i) false l_v2i) then
          TEBot, TEBot
        else
          if opt_o then ve1o, ve2o else
            let prepare_veo veo = 
              let vals = 
                if io_effects () then 
                  (List.combine l_z l_e2i)
                  |> List.map (fun (z, e2i) -> get_effmap e2i |> get_input_eff_relations z)
                  |> List.concat
                else []
              in
              List.combine l_z l_v2i
              |> List.fold_left (fun veo (z, v2i) -> arrow_VE z veo v2i) veo
              |> fun ve -> (
                if io_effects () then
                  let l = List.map (fun v -> stren_VE_Eff ve (Relation v)) vals in
                  List.fold_left (fun ve1 ve2 -> join_VE_Eff ve1 ve2) (List.hd l) (List.tl l)
                else ve)
            in
            let ve1 = prepare_veo ve1o in
            let ve2 = prepare_veo ve2o in
            if ve_have_effects () && extract_eff ve1 |> is_bot_Eff && (extract_v ve1 |> only_shape_V |> not)
            then TEBot, TEBot else prop ve1 ve2
      in
        
      let ve2o' = 
        if extract_v ve2o' |> only_shape_V |> not && extract_eff ve2o' |> is_bot_Eff
          then temap (id, 
            (fun _ -> 
              List.combine l_z l_v2i
              |> List.fold_left (fun e2i (z, v2i) ->
                arrow_EffV z e2i v2i) (last_element l_e2i)
              ))
            ve2o'
          else ve2o'
      in

      ve1o', ve2o'
    in
    let ve1o', ve2o' = 
      if opt_o then ve1o', ve2o' else (join_VE ve1o ve1o', join_VE ve2o ve2o')
    in
    l_ve1i', l_ve2i', ve1o', ve2o'
  in
  (l_ve1i', ve1o'), (l_ve2i', ve2o')

let multiprop_io p = measure_call "multiprop_io" (multiprop_io p)
        
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

let get_env_list (env: env_t) (m: exec_map_t) = 
    (* let find n m = NodeMap.find_opt n m |> Opt.get_or_else TEBot in *)
    let env_l = VarMap.bindings env in
    let helper lst (x, (n, _)) =
      (* let n = construct_snode trace n in
      let te = find n m in
      let lst' = match te with 
        | TypeAndEff (t, _) -> if is_List t then 
                                (get_item_var_V t) :: (get_len_var_V t) :: lst
                              else lst
        | _ -> lst
      in *)
      x :: lst
    in
    (List.fold_left helper [] env_l) @ (AbstractEv.spec_env_vars ())

let get_env_list e = measure_call "get_env_list" (get_env_list e)

let get_spec_acc_qset () = 
  if io_effects () then (AbstractEv.acc_vars (), AbstractEv.spec_qset ()) 
  else ([], [])

let rec add_tuple_to_env x tup_ve l trace env =
  let list_ve = get_tuple_list_VE tup_ve in
  List.fold_left (fun env' (ve, i) ->
    let x_i = x^"."^(string_of_int i) in
    let nx_i = construct_vnode env l trace in
    env' |> VarMap.add x_i (nx_i, false) |>
    if is_tuple_VE ve then
      add_tuple_to_env x_i ve l trace
    else
      id
    ) env (zip_list list_ve (List.length list_ve |> first_n))

let prop_scope (env1: env_t) (env2: env_t) (m: exec_map_t) (ve1: value_te) (ve2: value_te): (value_te * value_te) = 
    let env1 = get_env_list env1 m in
    let env2 = get_env_list env2 m in
    let ve1', ve2' = prop ve1 ve2 in
    let ve1'' = proj_VE ve1' (get_spec_acc_qset ()) env1 in
    let ve2'' = proj_VE ve2' (get_spec_acc_qset ()) env2 in
    (* (if !debug then
     let pr = Format.fprintf Format.std_formatter in
     pr "@.Prop Scope:@[<2>@[<v>@[ve1': @[%a@]@],@,@[ve2': @[%a@]@]@]@]
     ,@,@[ve1'': @[%a@]@]@]@],@,@[ve2'': @[%a@]@]@]@]"
       pr_value_and_eff ve1' pr_value_and_eff ve2' pr_value_and_eff ve1'' pr_value_and_eff ve2''); *)
    ve1'', ve2''

let prop_scope x1 x2 x3 x4 = measure_call "prop_scope" (prop_scope x1 x2 x3 x4)

(* let check_map m = NodeMap.iter (fun n ve ->
    match ve with
      | TEBot -> ()
      | TETop -> ()
      | TypeAndEff (v, e) -> 
          if is_Relation v || is_pure_Tuple ve then
            begin
              let vae = get_ae_from_v v in
              let eae = get_ae_from_eff (extract_ec ve) in
              if not ((eq_V vae eae) || (String.length (get_name n) > 4 && (String.equal "pref" (String.sub (get_name n) 0 4)))) then
                begin
                  Format.fprintf Format.std_formatter "@.node: @[%s@] @,ve: @[%a@]@." 
                  (get_label_snode n) pr_value_and_eff ve;
                  raise (Invalid_argument "value and effect should agree on ae")
                end
            end
  ) m *)

      
(** Reset the array nodes **)
let reset (m:exec_map_t): exec_map_t = NodeMap.fold (fun n te m ->
        m |> NodeMap.add n te
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

let rec list_var_item eis (cs: trace_t) m env nlst left_or_right lst_len = 
    let find n m = NodeMap.find_opt n m |> Opt.get_or_else TEBot in
    let vlst = find nlst m in
    match eis, left_or_right with
    | Var (x, l), true -> 
        let n = construct_vnode env l cs in
        let env1 = env |> VarMap.add x (n, false) in
        let n = construct_snode cs n in
        let te = find n m in
        let tep = cons_temp_lst_VE te vlst in
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
        let vlst', tep' = prop vlst tep in
        let te' = extrac_item_VE (get_env_list env1 m) tep' in
        (* (if !debug then
        begin
            Format.printf "\nRES for prop:\n";
            pr_value Format.std_formatter vlst';
            Format.printf "\n<<~~~~>>\n";
            pr_value Format.std_formatter t';
            Format.printf "\n";
        end
        ); *)
        let m' = m |> NodeMap.add n te' |> NodeMap.add nlst vlst' in
        m', env1
    | Var(x, l), false ->
        let n = construct_vnode env l cs in
        let env1 = env |> VarMap.add x (n, false) in
        let n = construct_snode cs n in
        let lst = find n m |> get_list_length_item_VE in
        let te_new = reduce_len_VE lst_len lst vlst in
        let _, te' = prop te_new (find n m) in
        let m' = NodeMap.add n te' m in
        m', env1
    | Const (_, l), false ->
        let n = l |> construct_enode env |> construct_snode cs in
        let te_new = pattern_empty_lst_VE vlst in
        let _, te' = prop te_new (find n m) in
        let m' = NodeMap.add n te' m in
        m', env
    | TupleLst (termlst, l), true -> 
        let n = l |> construct_enode env |> construct_snode cs in
        let te = 
            let raw_te = find n m in
            if is_tuple_VE raw_te then raw_te
            else 
              let u' = List.init (List.length termlst) (fun _ ->
                             TEBot) in (init_VE_v (Tuple u')) in
        (* (if !debug then
        begin
            Format.printf "\n<---Pattern tuple---> %s\n" l;
            pr_value Format.std_formatter vlst;
            Format.printf "\n<<~~~~>> %s\n" l;
            pr_value Format.std_formatter t;
            Format.printf "\n";
        end
        ); *)
        let telst = get_tuple_list_VE te in
        let teel = extrac_item_VE (get_env_list env m) vlst in
        let tellst = get_tuple_list_VE teel in
        let env', m', telst', tellst' = List.fold_left2 (fun (env, m, li, llst) e (tei, telsti) -> 
            match e with
            | Var (x, l') -> 
                let nx = construct_vnode env l' cs in
                let env1 = env |> VarMap.add x (nx, false) in
                let nx = construct_snode cs nx in
                let tex = find nx m in
                let tei', tex' = prop tei tex in
                let telsti', tei'' = prop telsti tei in
                let m' = m |> NodeMap.add nx tex' in
                env1, m', (join_VE tei' tei'') :: li, telsti' :: llst
            | _ -> raise (Invalid_argument "Tuple only for variables now")
        ) (env, m, [], []) termlst (zip_list telst tellst) in
        let telst', tellst' = List.rev telst', List.rev tellst' in
        let _, tee = destruct_VE te in 
        let _, teele = destruct_VE teel in
        let teele', tee' = prop_eff teele tee in
        let te', vlst' = (TypeAndEff ((Tuple telst'), tee')), 
                         (cons_temp_lst_VE (TypeAndEff ((Tuple tellst'), teele')) vlst) in
        let m'' = m' |> NodeMap. add n te'
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
    | BinOp (_, e1, e2, _), _ ->
        let m', env' = list_var_item e1 cs m env nlst true 0 in
        let m'', env'' = list_var_item e2 cs m' env' nlst false (lst_len + 1) in
        m'', env''
    | UnOp (_, e1, _), true ->
        let m', env' = list_var_item e1 cs m env nlst true 0 in
        m', env'
    | _ -> raise (Invalid_argument "Pattern should only be either constant, variable, and list cons")


(* let rec prop_predef l ve0 ve =
  match ve0, ve with 
  | TypeAndEff (v0, _), TypeAndEff (v, e) -> TypeAndEff ((prop_predef_v l v0 v), e)
  | _, _ -> ve
and prop_predef_v l v0 v = 
    let l = l |> name_of_node |> create_singleton_trace_call_loc in
    let rec alpha_rename ve0 ve = 
      match ve0, ve with
      | (TypeAndEff (v0, _)), (TypeAndEff (v, e)) -> TypeAndEff ((alpha_rename_v v0 v), e)
      | _, _ -> ve
    and alpha_rename_v v0 v = 
        match v0, v with
        | Table t0, Table t -> 
            if table_isempty t || table_isempty t0  then Table t else
            let trace0, (_, fout0), _ = get_full_table_T t0 in
            let _, veo0 = get_full_fout fout0 in
            let trace, (vei, fout), env = get_full_table_T t in
            let _, veo = get_full_fout fout in
            let veo' = alpha_rename veo0 
                         (alpha_rename_VE veo (get_trace_data trace) (get_trace_data trace0)) 
            in
            let t' = construct_table trace0 (vei, construct_fout create_empty_trace veo') env in
            Table t'
        | _, _ -> v      
    in
    match v0, v with
    | Table t0, Table t -> 
        let cs0, _, _ = get_full_table_T t0 in
        let vpdef = Table (construct_table cs0 (get_table_by_cs_T cs0 t) init_Env) in
        let pdefvp = ref (construct_table cs0 (get_table_by_cs_T cs0 t) init_Env) in
        let t' = table_mapi (fun cs (vei, veo) -> 
            if cs = cs0 || (not (is_subtrace l cs)) then vei, veo else
            let vs = Table (construct_table cs (vei, veo) init_Env) in
            let v' = vs |> alpha_rename_v v0 in
            let vt, v'' = prop_v vpdef v' in
            pdefvp := (match join_V vt (Table !pdefvp) with 
                      | Table t -> t 
                      | _ -> raise (Invalid_argument "Should be a table"));
            let vei', veo' = match alpha_rename_v vs v'' with
                | Table t'' -> let _, (vei, veo), _ = get_full_table_T t'' in (vei, veo)
                | _ -> raise (Invalid_argument "Expect predefined functions as table")
            in vei', veo') t
        in Table (
            let _, veio, _ = get_full_table_T !pdefvp in
            t' |> update_table cs0 veio
        )
    | _, _ -> v *)

let rec step term (env: env_t) (trace: trace_t) (ec: effect_t) (ae: value_tt) (assertion: bool) (is_rec: bool) (m:exec_map_t) (prev_m:exec_map_t): exec_map_t * trace_t list =
  let find n m = NodeMap.find_opt n m |> Opt.get_or_else TEBot in
  (* let find_ra q e = StateMap.find_opt q e |> Opt.get_or_else (bot_R Plus) in *)
  let update _ n v m =
    (*NodeMap.update n (function
        | None -> v
        | Some v' -> if false && widen then wid_V v' v else (*join_V v'*) v) m*)
      (* NodeMap.add n v m *)
    (* (if !debug && get_label_snode n = "EN: 31;z51__" then
      Format.fprintf Format.std_formatter "line 563. old_v: %a" pr_value_and_eff (find n m)); *)
    NodeMap.update n (fun old_v -> match old_v with None -> v | Some v' -> join_VE v' v) m
    (* (if !debug && get_label_snode n = "EN: 31;z51__" then
      Format.fprintf Format.std_formatter "line 563. new_v: %a" pr_value_and_eff (find n m'));
    m' *)
  in
  let check_mod_const e_mod env trace m = 
    let node_n = (loc e_mod |> construct_enode env |> construct_snode trace) in
    let val_n = proj_V (find node_n m |> extract_v) (get_spec_acc_qset ()) [] in
    let val_2 = init_V_c (Integer 2) in
    (* if !debug then Format.fprintf Format.std_formatter "@.Line 570: val1: %a; val2: %a"
      pr_value val_n
      pr_value val_2; *)
    let res = eq_V val_2 val_n in 
    (* if !debug then Format.fprintf Format.std_formatter "res %s" (string_of_bool res); *)
    res
  in
  let init_VE_wec v = TypeAndEff (v, (Effect ec)) in
  let merge_traces e tails m = 
    List.fold_left 
      (fun te_acc tail -> 
        let trace = extend_trace tail trace in 
        let ne = loc e |> construct_enode env |> construct_snode trace in
        let tee = find ne m in
        join_VE tee te_acc
      ) TEBot tails
  in
  if only_shape_V ae then m, [create_empty_trace] else begin
  (if !debug then Format.fprintf Format.std_formatter "%s_%s start\n" (loc term) (get_trace_data trace));
  let m, tails =
  match term with
  | Const (c, _) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Const ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end
      ); *)
      (* if optmization m n find then m else *)
      let n = loc term |> construct_enode env |> construct_snode trace in
      let te = find n m in (* M[env*l] *)
      let update_t t =  
        if leq_V ae t then t
        else
          let ct = init_V_c c in
          let t' = join_V t ct in
          (stren_V t' ae)
      in
      let update_e eff = join_Eff eff (stren_Eff (Effect ec) ae) in
      let te' = temap (update_t, update_e) te in
      m |> update false n te', [create_empty_trace] (* {v = c ^ aE}*)
  | NonDet _ ->
      let t = Relation (Bool (AbstractValue.from_int 1, AbstractValue.from_int 0)) in
      let t' = stren_V t ae in
      let te = TypeAndEff (t', Effect ec) in
      let n = loc term |> construct_enode env |> construct_snode trace in
      update false n te m, [create_empty_trace]
  | Var (x, l) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Var ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end); *)
      (if (!debug) then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 586, Var[%s], trace: %s, @,ec: @[%a@]@, @,ae: @[%a@]@."
           x (get_trace_data trace) pr_eff_map ec pr_value ae); 
      let n = loc term |> construct_enode env |> construct_snode trace in
      let (nx, _) = VarMap.find x env in
      let envx, lx, _ = get_vnode nx in
      let nx = construct_snode trace nx in      
      (* (if !debug && (x="x1") then
         find nx m |> (fun tex0 ->
         Format.fprintf Format.std_formatter "@.Pre_prog -> Var[%s]: @[%a@], @env: @[%a@]@." 
         x 
         pr_value_and_eff tex0 
         pr_env_vars (VarMap.bindings envx))
      ); *)

      let tex = find nx m in
      if get_ae_from_ve tex |> stren_V ae |> only_shape_V then m, []
      else
        let effx0 = tex |> extract_eff in
        let tex = find nx m 
                  |> temap ((fun tx -> 
                                    if is_Relation tx || is_tuple_V tx then
                                      if sat_equal_V tx x then tx
                                      else equal_V (forget_V x tx) x
                                    else tx),
                            (fun _ -> Effect ec))
        in
  
        let te = find n m in  (* M[env*l] *)
        (* (if !debug && (x="compute") then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 602, Var[%s](prop-pre), trace: %s, @,te: @[%a@]@."
            x (get_trace_data trace) pr_value_and_eff te); *)
        (if !debug then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 838, Var, cs_trace: %s,@,te1: @[%a@]@,te: @[%a@]@,ec1: @[%a@]@."
            (loc term)
            pr_value_and_eff tex
            pr_value_and_eff te
            pr_eff_map ec
        );
        let tex', te' = 
          (* if List.mem lx !pre_def_func then
            let tex0 = find nx m0 in
            let tex = if !trace_len > 0 then prop_predef l tex0 tex else tex in
            prop tex te
          else *)
            prop_scope envx env m tex te
        in
        (if !debug then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 838, Var, cs_trace: %s,@,te1: @[%a@]@,ec1: @[%a@]@."
            (get_trace_data trace)
            pr_value_and_eff tex'
            pr_value_and_eff te'
        );
        (* (if !debug && (l = "34" || l = "35") then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 610, Var[%s], Pre_stren, trace: %s, @,tex': @[%a@]@,te': @[%a@]@."
            x (get_trace_data trace)
            pr_value_and_eff tex'
            pr_value_and_eff te'); *)
        (* (if !debug && (x="compute") then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 630, Var[%s](prop-post), trace: %s, @,te': @[%a@]@, @,tex': @[%a@]@."
            x (get_trace_data trace) pr_value_and_eff te' pr_value_and_eff tex'); *)
        let tex' = temap (id, (fun _ -> effx0)) tex' in
        let te' = stren_VE te' ae in
        (if !debug then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 838, Var, cs_trace: %s,@,te1: @[%a@]@,ec1: @[%a@]@."
            (loc term)
            pr_value_and_eff tex'
            pr_value_and_eff te'
        );
        (* (if !debug && (l = "34" || l = "35") then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 610, Var[%s], Post_stren, trace: %s, @,tex': @[%a@]@,te': @[%a@]@."
            x (get_trace_data trace)
            pr_value_and_eff tex'
            pr_value_and_eff te'); *)
        (* (if !debug then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 707, Var[%s](prop-post), trace: %s, @,te': @[%a@]@, @,tex': @[%a@]@."
            x (get_trace_data trace) pr_value_and_eff te' pr_value_and_eff tex'); *)
        m |> update false nx tex' |> update false n te', [create_empty_trace] (* t' ^ ae *)
  | App (e1, e2, _) ->
    if !quick_prop then failwith "Entered App case with quick prop on";
      (* (if !debug then
      begin
          Format.printf "\n<=== App ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end);
      (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 642, App, trace: %s, @,ec: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec); *)
      let m0, tails1 = step e1 env trace ec ae assertion is_rec m prev_m in
      let map, tails = List.fold_left_map 
        (fun m0' tail1 -> 
          let trace1 = extend_trace tail1 trace in
          let n1 = loc e1 |> construct_enode env |> construct_snode trace1 in
          let te0 = find n1 m0 in
          let te1, m1 = te0, m0 in
          let ae1 = get_ae_from_ve te1 in
          if ((extract_v te1) = Bot) then join_M m0' m1, [tail1]
          else             
            if not @@ is_table (extract_v te1) then
              begin
                Format.printf "Error at location %s: expected function, but found %s.\n"
                  (loc e1) (string_of_value_and_eff te1);
                let trace = extend_trace tail1 trace in
                let n = loc term |> construct_enode env |> construct_snode trace in
                join_M m0' m1 |> update false n TETop, [tail1]
              end
            else
              (* let a = 0 in
              (if !debug then
                 let pr = Format.fprintf Format.std_formatter in
                 pr "\nLINE 645, App, te1: @[%a@]@." pr_value_and_eff te1); *)
              let ec' = extract_ec te1 in
              let ec' = stren_Eff (Effect ec') ae1 |> get_effmap in (* TODO: try removing this line *)
              (* (if true then
                 let pr = Format.fprintf Format.std_formatter in
                 pr "@.LINE 688, App, loc: %s @,te1:@[%a@]@,@,ec':@[%a@]@,@,ae1:@[%a@]@."
                 (loc e1)
                 pr_value_and_eff te1 pr_eff_map ec' pr_value ae1
              ); *)
              let m2, tails2 = step e2 env trace1 ec' ae1 assertion is_rec m1 prev_m in
              let map, tails = List.fold_left_map 
                (fun m2' tail2 -> 
                  let extended_tail = extend_trace tail2 tail1 in
                  let trace2 = extend_trace tail2 trace1 in
                  let n = loc term |> construct_enode env |> construct_snode trace2 in
                  let n2 = loc e2 |> construct_enode env |> construct_snode trace2 in
                  let te2 = find n2 m2 in (* M[env*l2] *)
                  let ae2 = get_ae_from_ve te2 in
                  (* (if !debug then
                     let pr = Format.fprintf Format.std_formatter in
                     pr "@.LINE 659, App, trace: %s,@,te2:@[%a@]@."
                       (get_trace_data trace2) pr_value_and_eff te2
                  ); *)
                  match te1, te2 with
                  | TEBot, _ | _, TEBot | _ when is_bot_VE te1 || is_bot_VE te2 -> m2', [extended_tail]
                  | _, TETop | _ when is_top_VE te2 -> m2' |> update false n TETop, [extended_tail]
                  | _ ->
                    begin
                      let cs_trace =
                        if !trace_len > 0 then
                          if is_rec && is_func e1 then trace2
                          else append_call_trace (loc e1 |> name_of_node) trace2
                        else dx_T te1
                      in
                      let te2' = 
                        te2
                        |> temap (id, fun eff -> arrow_EffV (get_trace_data cs_trace) eff (extract_v te2)) 
                      in
                      let fout_temp, fout_tails = 
                        if get_table_T te1 |> trace_exists cs_trace then
                          let _, fout = io_T cs_trace te1 in
                          let fout_tails = get_fout_tails fout in
                          List.fold_left (fun fout_temp tail ->
                            let trace3 = extend_trace tail trace2 in
                            let n = loc term |> construct_enode env |> construct_snode trace3 in
                            let te = find n m2' in
                            update_fout tail te fout_temp
                          ) init_fout fout_tails, fout_tails
                        else init_fout, [create_empty_trace]
                      in
                      let te_temp = Table (construct_table cs_trace (te2', fout_temp) (get_Relation ae2)) |> init_VE_v in
                      (* (if true then
                        let pr = Format.fprintf Format.std_formatter in
                        pr "@.LINE 721, App, cs_trace: %s,@,te1: @[%a@]@,te_temp: @[%a@]@."
                          (get_trace_data cs_trace)
                          pr_value_and_eff te1
                          pr_value_and_eff te_temp
                      ); *)
                      let te1', te0 = 
                        (* if optmization m2 n1 find && optmization m2 n2 find && optmization m2 n find then t1, t_temp else *)
                        prop te1 te_temp
                      in
                      
                      let te2', raw_te' = 
                        (* if is_array_set e1 then
                          let t2', t' = io_T cs t0 in
                          let elt = proj_V (get_second_table_input_V t') [] in
                          join_for_item_V t2' elt, t'
                          else  *)
                        io_T cs_trace te0
                      in
                      let te2' = get_env_list env m2' |> proj_VE te2' (get_spec_acc_qset ()) in
                      let m2' = m2' |> update false n1 te1' |> update false n2 te2' in
                      List.fold_left_map (fun m3' fout_tail ->
                        let extended_tail' = extend_trace fout_tail extended_tail in
                        let trace3 = extend_trace fout_tail trace2 in
                        let n = loc term |> construct_enode env |> construct_snode trace3 in
                        let raw_te'_tr = v_fout fout_tail raw_te' in
                        let te' = get_env_list env m3' |> proj_VE raw_te'_tr (get_spec_acc_qset ()) in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 691, App, cs_trace: %s,@,raw_te': @[%a@],@,te1': @[%a@]@,te2': @[%a@]@,te': @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff raw_te'_tr
                            pr_value_and_eff te1'
                            pr_value_and_eff te2'
                            pr_value_and_eff te'
                        );
                        let ae2' = extract_v te' |> get_ae_from_v in
                        let ae3 = if only_shape_V ae2' then ae2 else meet_V ae2 ae2' in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 691, App, cs_trace: %s,@,new te': @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff (stren_VE te' ae3)
                        );
                        m3' |> update false n (stren_VE te' ae3), (extended_tail')
                      ) m2' fout_tails
                      (* if is_array_set e1 && only_shape_V t2' = false then
                        let nx = VarMap.find (get_var_name e2) env in
                        let envx, lx,_ = get_vnode nx in
                        let nx = construct_snode sx nx in
                        let tx = find nx m in
                        let tx', t2' = prop_scope envx env sx res_m t2' tx in
                        res_m |> NodeMap.add n2 t2' |> NodeMap.add nx tx'
                        else  *)
                    end
                ) m2 tails2
              in
              join_M m0' map, sort_uniq_traces tails
        ) m0 tails1 
      in
      map, sort_uniq_traces tails
  | MultiApp (e1, termlist, _) ->
    let prepare_veo z vei veo =
      let vi, _ = match vei with
        | TEBot -> Bot, EffBot
        | TypeAndEff (v, e) -> v, e
        | TETop -> Top, EffTop 
      in
      arrow_VE z veo vi
      |> fun ve -> (
        if io_effects () then
          let ei = extract_eff vei in
          let vals = get_effmap ei |> get_input_eff_relations z in
          let l = List.map (fun v -> stren_VE_Eff ve (Relation v)) vals in
          List.fold_left (fun ve1 ve2 -> join_VE_Eff ve1 ve2) (List.hd l) (List.tl l)
        else ve)
    in
    let prepare_t cs vei ae ec tr veo =
      let fout = init_fout |> update_fout tr veo in
      let v = Table (construct_table cs (vei, fout) (get_Relation ae)) in
      TypeAndEff (v, ec)
    in
    let null_step_trace trace =
      (TEBot, Relation (bot_Env), ec, id, id, create_empty_trace, trace)
    in
    let rec partial_prop m_fun e_fun termlist step_traces =
      match termlist with
      | [] -> failwith "Empty termlist"
      | (e_app,e_arg)::termlist ->
        let m_term, step_traces = List.fold_left_map
          (fun m_fun' (te_fun, ae_fun, ec_fun, prepare_t', prepare_veo', trace_e1, tail_fun) ->
            let trace_fun = extend_trace tail_fun trace in
            if ((extract_v te_fun) = Bot) then 
              let te_fun' = prepare_t' TEBot in
              let n_e1 = loc e1 |> construct_enode env |> construct_snode trace_e1 in
              let m_fun' = m_fun' |> update false n_e1 te_fun' in
              m_fun', []
            else
              if not @@ is_table (extract_v te_fun) then
                begin
                  Format.printf "Error at location %s: expected function, but found %s.\n"
                    (loc e_fun) (string_of_value_and_eff te_fun);
                  let te_fun' = prepare_t' TEBot in
                  let n_e1 = loc e1 |> construct_enode env |> construct_snode trace_e1 in
                  let m_fun' = m_fun' |> update false n_e1 te_fun' in
                  m_fun', []
                end
              else
                let m_arg, tails_arg = step e_arg env trace_fun ec_fun ae_fun assertion is_rec m_fun' prev_m in
                let tails_arg = List.map (fun tail -> extend_trace tail tail_fun) tails_arg in
                let m_app, step_traces = List.fold_left_map
                  (fun m_arg' tail_arg ->
                    let trace_arg = extend_trace tail_arg trace in
                    let n_arg = loc e_arg |> construct_enode env |> construct_snode trace_arg in
                    let te_arg = find n_arg m_arg in
                    let ae_arg = get_ae_from_ve te_arg in

                    match te_fun, te_arg with
                    | TEBot, _ | _, TEBot | _ when is_bot_VE te_fun || is_bot_VE te_arg -> 
                      m_arg', []
                    | _, TETop | _ when is_top_VE te_arg -> m_arg', []
                    | _ ->
                      let cs_trace =
                        if !trace_len > 0 then
                          if is_rec && is_func e_fun then trace_arg
                          else append_call_trace (loc e_fun |> name_of_node) trace_arg
                        else dx_T te_fun
                      in
                      (if !debug then
                        let pr = Format.fprintf Format.std_formatter in
                        pr "@.LINE 721, App, cs_trace: %s,@,te_arg: @[%a@]@,te_fun: @[%a@]@."
                          (loc e_arg)
                          pr_value_and_eff te_arg
                          pr_value_and_eff te_fun
                      );
                      let te_arg' = 
                        te_arg
                        |> temap (id, fun eff -> arrow_EffV (get_trace_data cs_trace) eff (extract_v te_arg)) 
                      in
                      let prepare_veo'' = fun ve -> prepare_veo (get_trace_data cs_trace) te_arg' (prepare_veo' ve) in

                      match termlist with
                      | [] ->
                        let fout_temp, fout_tails = 
                          if get_table_T te_fun |> trace_exists cs_trace then
                            let _, fout = io_T cs_trace te_fun in
                            let fout_tails = get_fout_tails fout in
                            List.fold_left (fun fout_temp tail ->
                              let trace_term = extend_trace tail trace_arg in
                              let n_term = loc term |> construct_enode env |> construct_snode trace_term in
                              let te_term = find n_term m_arg' in
                              update_fout tail te_term fout_temp
                            ) init_fout fout_tails, fout_tails
                          else init_fout, [create_empty_trace]
                        in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 721, App, cs_trace: %s,@,te_arg': @[%a@]@,fout_temp: @[%a@]@."
                            (loc e_arg)
                            pr_value_and_eff te_arg'
                            pr_fout fout_temp
                        );

                        let te_fun_temp = 
                          Table (construct_table cs_trace (te_arg', fout_temp) (get_Relation ae_arg)) |> init_VE_v
                          |> prepare_veo'
                          (* |> fun ve -> (
                            if io_effects () then
                              let z = get_trace_data cs_trace in
                              let ei = extract_eff te_arg' in
                              let vals = get_effmap ei |> get_input_eff_relations z in
                              let l = List.map (fun v -> stren_VE_Eff ve (Relation v)) vals in
                              List.fold_left (fun ve1 ve2 -> join_VE_Eff ve1 ve2) (List.hd l) (List.tl l)
                            else ve) *)
                        in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 721, App, cs_trace: %s,@,te_fun_temp: @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff te_fun_temp
                        );
                        let te_fun', te_fun_temp' = 
                          prop te_fun te_fun_temp
                        in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 721, App, cs_trace: %s,@,te_fun_temp': @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff te_fun_temp'
                        );
                        let te_arg', te_app_temp = 
                          io_T cs_trace te_fun_temp'
                        in
                        let te_fun'' = prepare_t' te_fun' in
                        let n_e1 = loc e1 |> construct_enode env |> construct_snode trace_e1 in
                        let te_arg' = get_env_list env m_arg' |> proj_VE te_arg' (get_spec_acc_qset ()) in
                        let m_arg' = m_arg' |> update false n_arg te_arg' |> update false n_e1 te_fun'' in

                        List.fold_left_map (fun m_app' fout_tail ->
                          let tail_app = extend_trace fout_tail tail_arg in
                          let trace_term = extend_trace fout_tail trace_arg in
                          let n_term = loc term |> construct_enode env |> construct_snode trace_term in

                          let te_app_tr = v_fout fout_tail te_app_temp in
                          let te_app = get_env_list env m_arg' |> proj_VE te_app_tr (get_spec_acc_qset ()) in
                          let ae_app = extract_v te_app |> get_ae_from_v in
                          let ae_app' = if only_shape_V ae_app then ae_arg else meet_V ae_arg ae_app in
                          update false n_term (stren_VE te_app ae_app') m_app',
                            Some (null_step_trace tail_app)
                        ) m_arg' fout_tails
                      | _ ->
                        let fout_temp, fout_tails = 
                          if get_table_T te_fun |> trace_exists cs_trace then
                            let _, fout = io_T cs_trace te_fun in
                            let fout_tails = get_fout_tails fout in
                            List.fold_left (fun fout_temp tail ->
                              update_fout tail TEBot fout_temp
                            ) init_fout fout_tails, fout_tails
                          else init_fout, [create_empty_trace]
                        in
                        (* (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 721, App, cs_trace: %s,@,te1: @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff te_arg'
                        ); *)
                        let te_fun_temp = 
                          Table (construct_table cs_trace (te_arg', fout_temp) (get_Relation ae_arg)) |> init_VE_v
                          |> prepare_veo'
                        in
                        (* (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 721, App, cs_trace: %s,@,te1: @[%a@]@,te_temp: @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff te_fun
                            pr_value_and_eff te_fun_temp
                        ); *)
                        let te_fun', te_fun_temp' = 
                          prop te_fun te_fun_temp
                        in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 721, App, cs_trace: %s,@,te_fun': @[%a@]@,te_fun_temp': @[%a@]@."
                            (get_trace_data cs_trace)
                            pr_value_and_eff te_fun'
                            pr_value_and_eff te_fun_temp'
                        );
                        let te_arg', te_app_temp = 
                          io_T cs_trace te_fun_temp'
                        in
                        let te_arg_fun, te_app_fun =
                          io_T cs_trace te_fun'
                        in
                        let ae_fun = get_ae_from_ve te_fun' in
                        let ec_fun = Effect (extract_ec te_fun') in
                        let te_arg' = get_env_list env m_arg' |> proj_VE te_arg' (get_spec_acc_qset ()) in
                        let m_arg' = m_arg' |> update false n_arg te_arg' in

                        List.fold_left_map (fun m fout_tail ->
                          let tail_app = extend_trace fout_tail tail_arg in

                          let te_app_tr = v_fout fout_tail te_app_temp in
                          let te_app_temp = get_env_list env m_arg' |> proj_VE te_app_tr (get_spec_acc_qset ()) in
                          if only_shape_VE te_app_temp then
                            let te_fun'' = prepare_t' te_fun' in
                            let n_e1 = loc e1 |> construct_enode env |> construct_snode trace_e1 in
                            update false n_e1 te_fun'' m_arg', None
                          else
                            let prepare_t'' =
                              fun veo -> prepare_t cs_trace te_arg_fun ae_fun ec_fun fout_tail veo |> prepare_t'
                            in
                            let ae_app = get_ae_from_ve te_app_temp in
                            let ae_app' = meet_V ae_arg ae_app in
                            let ec_app = stren_Eff (Effect (extract_ec te_app_temp)) ae_app' |> get_effmap in
                            
                            (if !debug then
                              let pr = Format.fprintf Format.std_formatter in
                              pr "@.LINE 721, App, cs_trace: %s,@,ae: @[%a@]@,ec: @[%a@]@."
                                (get_trace_data cs_trace)
                                pr_value ae_app'
                                pr_eff_map ec_app
                            );
                            let te_app = v_fout fout_tail te_app_fun in
                            (* let te_app = stren_VE te_app_temp ae_app' in *)
                            m, Some (te_app, ae_app', ec_app, prepare_t'', prepare_veo'', trace_e1, tail_app)
                        ) m_arg' fout_tails
                  ) m_arg tails_arg
                in
                m_app, List.flatten step_traces
          ) m_fun step_traces
        in
        let step_traces = 
          List.flatten step_traces |> List.filter_map id
          |> List.sort_uniq (fun (_,_,_,_,_,_,trace1) (_,_,_,_,_,_,trace2) -> comp_trace trace1 trace2) 
        in
        match termlist with
        | [] -> m_term, List.map (fun (_,_,_,_,_,_,trace) -> trace) step_traces
        | _ -> partial_prop m_term e_app termlist step_traces
    in
    let m1, tails1 = step e1 env trace ec ae assertion is_rec m prev_m in
    let step_traces = 
      List.map 
        (fun tail1 -> 
          let trace1 = extend_trace tail1 trace in
          let n1 = loc e1 |> construct_enode env |> construct_snode trace1 in
          let te1 = find n1 m1 in
          let ae1 = get_ae_from_ve te1 in
          let ec1 = extract_ec te1 in
          (if !debug then
            let pr = Format.fprintf Format.std_formatter in
            pr "@.LINE 721, App, cs_trace: %s,@, node: %s,@,te1: @[%a@]@,ec1: @[%a@]@."
              (loc e1)
              (get_label_snode n1)
              pr_value_and_eff te1
              pr_eff_map ec1
          );
          te1, ae1, ec1, id, id, trace1, tail1
        ) tails1
    in
    partial_prop m1 e1 termlist step_traces
  | BinOp (bop, e1, e2, _) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Binop ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end); *)
      (* (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 748, BinOp, trace: %s, @,ec: @[%a@]@, @,ae: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec pr_value ae); *)
      let n1 = loc e1 |> construct_enode env |> construct_snode trace in
      let m1, tails1 =
        match bop with
        | Cons ->
            let rec cons_list_items ec m term = match term with (* 1 :: 2 :: 3 ::  *)
              | BinOp (Cons, e1', e2', _) ->
                  let l1, te1', m1' = cons_list_items ec m e1' in
                  if is_bot_VE te1' then l1, te1', m1'
                  else
                    let ec' = extract_ec te1' in (*TODO needs fixing*)
                    let l2, te2', m2' = cons_list_items ec' m1' e2' in
                    l1 + l2, join_VE te1' te2', m2'
              | _ ->
                  let m', tails = step term env trace ec ae assertion is_rec m prev_m in
                  let te' = merge_traces term tails m' in
                  1, te', m'
            in
            let _, te1', m1 = cons_list_items ec m e1 in
            let te1 = find n1 m1 in
            let te1', _ = prop te1 te1' in
            m1 |> update false n1 te1', [create_empty_trace]
        | _ -> step e1 env trace ec ae assertion is_rec m prev_m
      in
      let map, tails = 
        List.fold_left_map 
          (fun m1' tail1 ->
            let trace1 = extend_trace tail1 trace in
            let n1 = loc e1 |> construct_enode env |> construct_snode trace1 in
            let te1 = find n1 m1 in
            let ae1 = get_ae_from_ve te1 in
            (if !debug then
               let pr = Format.fprintf Format.std_formatter in
               pr "@.LINE 781, BinOp.pre-e2, trace: %s, te: @[%a@]"
                 (get_trace_data trace1) pr_value_and_eff te1);
            if is_bot_VE te1 then (m1', [tail1]) 
            else 
              begin
                let ec' = find n1 m1 |> extract_ec in
                let ec' = stren_Eff (Effect ec') ae1 |> get_effmap in (* TODO: try removing this line *)
                let m2, tails2 = step e2 env trace1 ec' ae1 assertion is_rec m1 prev_m in
                let te1 = find n1 m2 in
                let map, tails = 
                  List.fold_left_map 
                    (fun m2' tail2 ->
                      let trace2 = extend_trace tail2 trace1 in
                      let extended_tail = extend_trace tail2 tail1 in
                      let n2 = loc e2 |> construct_enode env |> construct_snode trace2 in
                      let n = loc term |> construct_enode env |> construct_snode trace2 in
                      let te2 = find n2 m2 in
                      let ae2 = get_ae_from_ve te2 in
                      if is_bot_VE te1 || is_bot_VE te2 then m2', extended_tail
                      else
                        begin
                          let bop =
                            match bop, e1, e2 with
                            | Mod, Const _, Const _ -> Mod
                            | Mod, _, Const _ -> Modc
                            | _ -> bop
                          in
                          let mod_eq_flag, e1, e2 = 
                            match bop, e1, e2 with
                            | Eq, BinOp (Mod, _, e_mod, _), _ -> check_mod_const e_mod env trace1 m1', e1, e2
                            | Eq, _, BinOp (Mod, _, e_mod, _) -> check_mod_const e_mod env trace2 m2', e2, e1
                            | Ne, BinOp (Mod, _, e_mod, _), _ -> check_mod_const e_mod env trace1 m1', e1, e2
                            | Ne, _, BinOp (Mod, _, e_mod, _) -> check_mod_const e_mod env trace2 m2', e2, e1
                            | _ -> false, e1, e2
                          in
                          let te = find n m2 in
                          let raw_te =
                            match bop with
                            | Cons (* list op *) ->
                               (* (pr_value_and_eff Format.std_formatter te1;
                                  pr_value_and_eff Format.std_formatter te2); *)
                               list_cons_VE te1 te2
                            | And | Or (* bool op *) ->
                               bool_op_VE bop te1 te2
                            | Modc ->
                               (* {v:int | a(t) ^ v = n1 mod const }[n1 <- t1] *)
                               let ted = init_VE_v (Relation (top_R bop)) in
                               let node_1 = e1 |> loc |> name_of_node in
                               let te' = arrow_VE node_1 ted (extract_v te1) 
                                         |> temap (id, fun _ -> extract_eff te1) in
                               let te''' = op_VE node_1 (str_of_const e2) bop mod_eq_flag te' 
                                           |> temap (id, fun _ -> extract_eff te2) in
                               let te = get_env_list env m2 |> proj_VE te''' (get_spec_acc_qset ()) in
                               te
                            | Seq ->
                               te2
                            | _ ->
                               (* {v:int | a(t) ^ v = n1 op n2 }[n1 <- t1, n2 <- t2] *)
                               let ted = init_VE_v (Relation (top_R bop)) in
                               let node_1 = e1 |> loc |> name_of_node in
                               let node_2 = e2 |> loc |> name_of_node in
                               let te' = arrow_VE node_1 ted (extract_v te1) |> temap (id, fun _ -> extract_eff te1) in
                               let te'' = arrow_VE node_2 te' (extract_v te2) |> temap (id, fun _ -> extract_eff te2) in
                               (* (if !debug then
                                  begin
                                  Format.printf "\n<=== Op binop ===> %s\n" (loc e1);
                                  pr_value Format.std_formatter t';
                                  Format.printf "\n<<~~~~>>\n";
                                  pr_value Format.std_formatter t'';
                                  Format.printf "\n";
                                  end
                                  ); *)
                               let te''' = op_VE node_1 node_2 bop mod_eq_flag te'' in
                               let temp_te = get_env_list env m2 |> proj_VE te''' (get_spec_acc_qset ()) in
                               (* if !domain = "Box" then
                                  temp_t |> der_V e1 |> der_V e2  (* Deprecated: Solve remaining constraint only for box*)
                                  else  *)
                               temp_te
                          in
                          let _, re_te =
                            if is_Relation (extract_v raw_te) then raw_te,raw_te else 
                              (* let t, raw_t = alpha_rename_Vs t raw_t in *)
                              prop raw_te te
                          in
                          (* (if l = "18" then
                             begin
                             Format.printf "\nRES binop for prop:\n";
                             pr_value Format.std_formatter re_t;
                             Format.printf "\n<-- ae -->\n";
                             pr_value Format.std_formatter ae;
                             Format.printf "\n";
                             end
                             ); *)
                          let m2'', re_te =
                            match bop with
                            | Cons ->
                               let te1l = cons_temp_lst_VE te1 re_te in
                               let te1l', re_te' = prop te1l re_te in
                               let te1' = extrac_item_VE (get_env_list env m2) te1l' in
                               let te2', _ = prop te2 re_te' in
                               m2' |> update false n1 te1' |> update false n2 te2', re_te'
                            | Seq ->
                               let te2', re_te' = prop te2 re_te in
                               m2' |> update false n2 te2', re_te' 
                            | _ -> m2', re_te 
                          in
                          (* (if !debug then
                             begin
                             Format.printf "\n<=== RES Op binop ===> \n node: %s" (get_label_snode n);
                             pr_value_and_eff Format.std_formatter (stren_VE re_te ae2);
                             pr_value_and_eff Format.std_formatter re_te;
                             Format.printf "\n";
                             end
                             ); *)
                          m2'' |> update false n (stren_VE re_te ae2), extended_tail
                        end
                    ) m2 tails2
                in
                join_M map m1', tails
              end
          ) m1 tails1
      in
      map, sort_uniq_traces tails
  | UnOp (uop, e1, _) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Unop ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end);
      (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 899, UnOp, trace: %s, @,ec: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec); *)
      let m1, tails = step e1 env trace ec ae assertion is_rec m prev_m in
      List.fold_left_map (fun m1' tail ->
        let trace = extend_trace tail trace in
        let n1 = loc e1 |> construct_enode env |> construct_snode trace in
        let n = loc term |> construct_enode env |> construct_snode trace in
        let te1 = find n1 m1 in
        let ae1 = get_ae_from_ve te1 in
        if is_bot_VE te1 then m1', tail
        else
          let node_1 = e1 |> loc |> name_of_node in
          let ted = init_VE_v (Relation (utop_R uop)) in
          let te = find n m1 in
          let te' = arrow_VE node_1 ted (extract_v te1) |> temap (id, fun _ -> extract_eff te1) in
          let te'' = uop_VE uop node_1 te' in
          let raw_te = get_env_list env m1 |> proj_VE te'' (get_spec_acc_qset ()) in
          (* if !domain = "Box" then
            temp_t |> der_V e1 |> der_V e2  (* Deprecated: Solve remaining constraint only for box*)
            else  *)
          let _, re_te =
            if is_Relation (extract_v raw_te)
            then raw_te, raw_te
            else prop raw_te te
          in
          m1' |> update false n (stren_VE re_te ae1), tail
      ) m1 tails
  | Ite (e0, e1, e2, _) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Ite ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end);
      (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 934, Ite, trace: %s, @,ec: @[%a@]@, @,ae: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec pr_value ae); *)
      let m0, tails0 = 
        (* if optmization m n0 find then m else  *)
        step e0 env trace ec ae assertion is_rec m prev_m in
      let map, tails = List.fold_left_map 
        (fun m0' (tail0) ->
          let trace0 = extend_trace tail0 trace in
          let n0 = loc e0 |> construct_enode env |> construct_snode trace0 in
          let te0 = find n0 m0 in
          let ae0 = get_ae_from_ve te0 in
          if is_bot_VE te0 then m0', [tail0] else
          if not @@ is_bool_V (extract_v te0) then 
            let n = loc term |> construct_enode env |> construct_snode trace0 in
            m0' |> update false n TETop, [tail0]
          else
            begin
              let t_true = extrac_bool_V (extract_v te0) true |> meet_V ae0 in (* Meet with ae*)
              (* (if !debug && get_label_snode n = "EN: 33;(z28*22-20)" then
                Format.fprintf Format.std_formatter
                "@.line 1047. tail1:@[%s@] te: @[%a@]@, te':@[%a@]@, te1: @[%a@]@, te1':@[%a@]"
              ); *)
              let t_false = extrac_bool_V (extract_v te0) false |> meet_V ae0 in
              let ec' = extract_ec te0 in
              let ec' = stren_Eff (Effect ec') ae0 |> get_effmap in (* TODO: try removing this line *)
              let ec_true = extrac_bool_V (extract_v te0) true |> stren_Eff (Effect ec') |> get_effmap in
              let ec_false = extrac_bool_V (extract_v te0) false |> stren_Eff (Effect ec') |> get_effmap in
              (* (if true then
                 Format.fprintf Format.std_formatter 
                   "@[<v>@[loc: @[%s@]@]\
                    @.EfectContext (ITE): @[<v>@[ec': @[%a@]@],@,\
                    @.AE (ITE): @[<v>@[ae': @[%a@]@],@,\
                    @,@[ec_true: @[%a@]@],@,@[ec_false: @[%a@]@]@]\
                    @,@[t_true: @[%a@]@],@,@[t_false: @[%a@]@]@]"
                  (loc term)
                   pr_eff_map ec'
                   pr_value ae0
                   pr_eff_map ec_true
                   pr_eff_map ec_false
                   pr_value t_true
                   pr_value t_false); *)

              let m1, tails1 = step e1 env trace ec_true t_true assertion is_rec m0 prev_m in
              let m2, tails2 = step e2 env trace ec_false t_false assertion is_rec m1 prev_m in
              let (map_true, te_true), tails1 = List.fold_left_map 
                (fun (m1', _) tail1 ->
                  let trace1 = extend_trace tail1 trace in
                  let tail1 = if !if_part && trace_isempty tail1 then append_part_trace (create_if_token (loc term) true) tail1 else tail1 in
                  let trace = extend_trace tail1 trace in
                  let n = loc term |> construct_enode env |> construct_snode trace in
                  let te = find n m0 in
                  let n1 = loc e1 |> construct_enode env |> construct_snode trace1 in
                  let te1 = find n1 m1 in
                  let te1', te' = 
                  (* if optmization m1 n1 find && optmization m1 n find then t1, (find n m1) else  *)
                  (* let t1, t = alpha_rename_Vs t1 t in *)
                    prop te1 te in
                  let te1' = stren_VE te1' t_true in
                  (* (if !debug && get_label_snode n = "EN: 74;(z180*)" then 
                    Format.fprintf Format.std_formatter
                    "@.line 1047. tail1:@[%s@] te: @[%a@]@, te':@[%a@]@, te1: @[%a@]@, te1':@[%a@]"
                    (get_trace_data tail1)
                    pr_value_and_eff te
                    pr_value_and_eff te'
                    pr_value_and_eff te1
                    pr_value_and_eff te1'); *)
                  let m1' = m1' |> update false n1 te1' in
                  ((if !trace_len > 0 && !if_part then m1' |> update false n te' else m1'), te1'), tail1
                ) (m2, TEBot) tails1
              in

              let (map_false, te_false), tails2 = List.fold_left_map
                (fun (m2', _) tail2 ->
                  let trace2 = extend_trace tail2 trace in
                  let tail2 = if !if_part && trace_isempty tail2 then append_part_trace (create_if_token (loc term) false) tail2 else tail2 in
                  let trace = extend_trace tail2 trace in
                  let n = loc term |> construct_enode env |> construct_snode trace in
                  let te = find n m0 in
                  let n2 = loc e2 |> construct_enode env |> construct_snode trace2 in
                  let te2 = find n2 m2 in
                  let te2', te' = 
                  (* if optmization m1 n1 find && optmization m1 n find then t1, (find n m1) else  *)
                  (* let t1, t = alpha_rename_Vs t1 t in *)
                    prop te2 te in
                  let te2' = stren_VE te2' t_false in
                  (* (if !debug && get_label_snode n = "EN: 33;(z28*22-20)" then 
                    Format.fprintf Format.std_formatter
                    "@.line 1074. trace:@[%s@] te: @[%a@]@, te':@[%a@]@, te2: @[%a@]@, te2':@[%a@]"
                    (get_trace_data tail2)
                    pr_value_and_eff te
                    pr_value_and_eff te'
                    pr_value_and_eff te2
                    pr_value_and_eff te2'); *)
                  let m2' = m2' |> update false n2 te2' in
                  ((if !trace_len > 0 && !if_part then m2' |> update false n te' else m2'), te2'), tail2
                ) (m2, TEBot) tails2
              in
              let n = loc term |> construct_enode env |> construct_snode trace in
              if !trace_len = 0 || not !if_part then
                let map = join_M map_true map_false in
                let ae0 =
                  if only_shape_VE te_true then
                    if only_shape_VE te_false then Bot
                    else t_false
                  else if only_shape_VE te_false then t_true
                  else ae0
                in
                let te_n' = join_VE (stren_VE te_true ae0) (stren_VE te_false ae0) in
                (* (if true then
                  let pr = Format.fprintf Format.std_formatter in
                  pr "@.LINE 1108, Ite, trace: %s, @,te_t: @[%a@]@, @,te_f: @[%a@]@, @,te_n': @[%a@]@, @,ae0: @[%a@]@."
                    (loc term) 
                    pr_value_and_eff te_true
                    pr_value_and_eff te_false
                    pr_value_and_eff te_n'
                    pr_value ae0); *)
                map |> update false n te_n', [create_empty_trace]
              else
                (* let tails1 = List.filter (fun (_, ae) -> not @@ only_shape_V ae) tails1 in if there's only one tail, update map *)
                (* let tails2 = List.filter (fun (_, ae) -> not @@ only_shape_V ae) tails2 in *)
                let map = join_M map_true map_false in
                join_M m0' map, tails1 @ tails2
            end
        ) m0 tails0
      in
      map, sort_uniq_traces tails
  | Rec (f_opt, (x, lx), e1, _) ->
    if !quick_prop then failwith "Entered Rec case with quick prop on";
      (* (if !debug then
      begin
          Format.printf "\n<=== Func ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end); *)
      (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 1010, Func, trace: %s, @,ec: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec);
      let n = loc term |> construct_enode env |> construct_snode trace in
      let te0 = find n m in
      (* (if !debug then
        let pr = Format.fprintf Format.std_formatter in
        pr "@.LINE 1020, Func, @,te0: @[%a@]@."
          pr_value_and_eff te0); *)
      let te =
        match te0 with
        | TEBot | _ when is_bot_VE te0 ->
            let tee =  init_T (append_call_trace (fresh_z ()) trace) (get_Relation ae) in
            Table tee |> init_VE_wec
        | _ -> temap (set_table_ae ae, fun _ -> Effect ec) te0
      in
      (* (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 994, Lambda[l=49](prop-pre), trace: %s, @,te: @[%a@]@."
           (get_trace_data trace)
           pr_value_and_eff te); *)
      let trace' = trace in
      let is_rec' = Opt.exist f_opt || is_rec in
      step_func (fun _ trace (tel, ter) m' -> 
          if is_bot_VE tel then m' |> update false n te
          else if is_top_fout ter then top_M m' else
          begin
            let z = get_trace_data trace in
            let f_nf_opt =
              Opt.map (fun (f, lf) -> f, (construct_vnode env lf trace', true)) f_opt
            in
            let trace' = if is_rec' && x = "_" then trace' else trace in
            let nx = construct_vnode env lx trace in
            let env' = 
              env |> VarMap.add x (nx, false) 
              |> 
                if io_effects () then add_input_acc_to_env x nx
                else id
            in
            let nx = construct_snode trace nx in
            let tex = find nx m in
            let env' = 
              if is_tuple_VE tel then
                add_tuple_to_env x tel lx trace env'
              else env' in
            (* (if !debug then 
              let pr = Format.fprintf Format.std_formatter in
              pr "@.LINE 1061, env: @[%a@]@."
              pr_env_vars (VarMap.bindings env')); *)
            let env1 =
              env' |>
              (Opt.map (uncurry VarMap.add) f_nf_opt |>
              Opt.get_or_else (fun env -> env))
            in
            let tails = get_fout_tails ter in
            let te1 = List.fold_left (fun fout tail ->
              let trace' = extend_trace tail trace' in
              let n1' = loc e1 |> construct_enode env1 |> construct_snode trace' in
              let te1 = (
                  if x = "_" then find n1' m else 
                  if is_tuple_VE tex then
                    get_tuple_list_VE tex |> List.length |> first_n |>
                    List.fold_left (fun v i ->
                      let z_i = z^"."^(string_of_int i) in
                      let x_i = x^"."^(string_of_int i) in
                      replace_VE v x_i z_i
                      ) (find n1' m)
                  else
                    replace_VE (find n1' m) x z
                )
                |> fun ve -> 
                  if io_effects () then
                    List.combine (acc_vars_list x) (acc_vars_list z)
                    |> List.fold_left (fun ve (old_var, new_var) -> replace_VE ve old_var new_var) ve
                  else ve
                in
                update_fout tail te1 fout
              ) init_fout tails in
            (* (if !debug && lx = "23" then 
              let pr = Format.fprintf Format.std_formatter in
              pr "@.LINE 1061, ae: @[%a@]@, te1: @[%a@]@."
                pr_value ae pr_fout te1); *)

            (* let tex = TypeAndEff (proj_V (extract_v tex) [] |> meet_V ae', extract_eff tex) in *)
            let prop_t = Table (construct_table trace (tex, te1) (get_Relation (get_ae_from_v (extract_v te)))) in
            let prop_te = TypeAndEff (prop_t, (extract_eff te)) in
            (* (if l = "20" then
              begin
              Format.printf "\n<=== Prop lamb ===> %s %s\n" lx (loc e1);
              pr_value Format.std_formatter prop_t;
              Format.printf "\n<<~~~~>> %s\n" l;
              pr_value Format.std_formatter t;
              end
              ); *)
            (*(if !debug && l = "49" then
               let pr = Format.fprintf Format.std_formatter in
               pr "@.LINE 1004, Lambda, trace: %s, @,prop_te: @[%a@]@,te: @[%a@]@."
                 (get_trace_data trace)
                 pr_value_and_eff prop_te
                 pr_value_and_eff te); *)
            let px_te, te1 = prop_scope env1 env' m prop_te te in
            (* (if !debug then 
              let pr = Format.fprintf Format.std_formatter in
              pr "@.LINE 1104, trace: %s, px_te: @[%a@]@, te1: @[%a@]@, prop_te: @[%a@]@, te: @[%a@]@."
                (get_trace_data trace)
                pr_value_and_eff px_te pr_value_and_eff te1 pr_value_and_eff prop_te pr_value_and_eff te); *)
            (* (if l = "20" then
              begin
              Format.printf "\nRES for prop:\n";
              pr_value Format.std_formatter px_t;
              Format.printf "\n<<~~~~>>\n";
              pr_value Format.std_formatter t1;
              Format.printf "\n";
              end
              ); *)
            let nf_t2_tf'_opt =
              Opt.map (fun (_, (nf, _)) ->
                let envf, _, _ = get_vnode nf in
                let nf = construct_snode trace nf in
                let tef = find nf m in
                (* (if lf = "4" then
                      begin
                          Format.printf "\n<=== Prop um ===> %s\n" l;
                          pr_value Format.std_formatter t;
                          Format.printf "\n<<~~~~>> %s\n" lf;
                          pr_value Format.std_formatter tf;
                          Format.printf "\n";
                      end
                ); *)
                (if !debug && (loc term)="92" then
                  let pr = Format.fprintf Format.std_formatter in
                  pr "@.LINE 1114, @,trace: @[%s@]@  @,te2: @[%a@]@  @,tef': @[%a@]@."
                    (get_trace_data trace) 
                    pr_value_and_eff te1
                    pr_value_and_eff tef);
                let te2, tef' = prop_scope env' envf m te1 tef in
                (if !debug && (loc term)="92" then
                  let pr = Format.fprintf Format.std_formatter in
                  pr "@.LINE 1114, @,trace: @[%s@]@  @,te2: @[%a@]@  @,tef': @[%a@]@."
                    (get_trace_data trace) 
                    pr_value_and_eff te2
                    pr_value_and_eff tef');
                (* let t2, tf' = prop t tf in *)
                (* (if !debug && lf = "8" then
                  begin
                  Format.printf "\nRES for prop:\n";
                  pr_value_and_eff Format.std_formatter te2;
                  Format.printf "\n<<~~~~>>\n";
                  pr_value_and_eff Format.std_formatter tef';
                  Format.printf "\n";
                  end
                  ); *)
                nf, te2, tef') f_nf_opt
            in
            let tex', te1' = io_T trace px_te in
            let m1 = m 
                     |> update is_rec' nx tex' 
                     |> (Opt.map (fun (nf, te2, tef') -> 
                            (* (if !debug && lx = "23" then
                              let pr = Format.fprintf Format.std_formatter in
                              pr "@.@.LINE 1308, Lambda(before eval body), trace:%s, @,tef': @[%a@]@." 
                                (get_trace_data trace') pr_value_and_eff tef');  *)
                             fun m' -> m' 
                                    |> update true nf tef' 
                                    |> update false n te2) nf_t2_tf'_opt 
                         |> Opt.get_or_else (update false n te1)) 
            in
            let m1 = List.fold_left (fun m1' tail -> 
                let trace' = extend_trace tail trace' in
                let n1' = loc e1 |> construct_enode env1 |> construct_snode trace' in
                let te1' = v_fout tail te1' in
                let te1' = (
                    if x = "_" then te1'
                    else
                      if is_tuple_VE tex then
                        get_tuple_list_VE tex |> List.length |> first_n |>
                        List.fold_left (fun v i ->
                          let z_i = z^"."^(string_of_int i) in
                          let x_i = x^"."^(string_of_int i) in
                          replace_VE v z_i x_i
                          ) te1'
                      else 
                        replace_VE te1' z x
                  )
                  |> fun ve -> 
                    if io_effects () then
                      List.combine (acc_vars_list z) (acc_vars_list x) 
                      |> List.fold_left (fun ve (old_var, new_var) -> replace_VE ve old_var new_var) ve
                    else ve
                in
                update false n1' te1' m1'
              ) m1 tails in
            (if !debug then
               let pr = Format.fprintf Format.std_formatter in
               pr "@.@.LINE 1259, Lambda(before eval body), trace:%s, @,tex': @[%a@]@." 
                 (get_trace_data trace') pr_value_and_eff tex'); 
            let ae'' = get_ae_from_v (extract_v tex') in
            let ae' = if (x <> "_" && is_Relation (extract_v tex')) || is_tuple_VE tex' || is_List (extract_v tex') then 
              (* if only_shape_V tx then ae else  *)
              (arrow_V x ae (extract_v tex')) else if only_shape_V ae'' then ae else stren_V ae ae'' (* TODO: try only ae'' *)  
            in
            let ec' = extract_ec tex' in
            let ec' = (
                if is_tuple_VE tex then
                  get_tuple_list_VE tex |> List.length |> first_n |>
                  List.fold_left (fun ec i ->
                    let z_i = z^"."^(string_of_int i) in
                    let x_i = x^"."^(string_of_int i) in
                    replace_Eff ec z_i x_i
                    ) (Effect ec')
                else
                  replace_Eff (Effect ec') z x
              )
              |> fun e -> stren_Eff e ae'
              |> get_effmap
              |> if io_effects () then StateMap.mapi (set_input_types_i x) else id
            in
            (if !debug then 
              let pr = Format.fprintf Format.std_formatter in
              pr "@.LINE 1068, ae': @[%a@]@, ec': @[%a@]@."
                pr_value ae' pr_eff_map ec');
            let m1', tails = step e1 env1 trace' ec' ae' assertion is_rec' m1 prev_m in
            (* (if !debug && lx = "97" then
              Format.fprintf Format.std_formatter "@.@.LINE 1301, %a" pr_tails tails); *)
            let m1' = 
              if is_rec' then
                let n1' = loc e1 |> construct_enode env1 |> construct_snode trace' in
                let body_val = List.fold_left 
                  (fun acc_val tail ->
                    let trace = extend_trace tail trace' in
                    let node = loc e1 |> construct_enode env1 |> construct_snode trace in
                    let node_val = find node m1' in
                    (* (if !debug && lx = "107" then
                      let pr = Format.fprintf Format.std_formatter in
                      pr "@.@.LINE 1150, Lambda(before eval body), trace:%s, @,node_val: @[%a@]@." 
                        (get_trace_data trace) pr_value_and_eff node_val); *)
                    join_VE acc_val node_val
                  ) TEBot tails in
                update false n1' body_val m1'
              else
                let tex,  _ = find n m1' |> io_T trace in
                let fout_temp = List.fold_left
                  (fun fout' tail ->
                    update_fout tail TEBot fout'
                  ) init_fout tails in
                let new_te = TypeAndEff (Table (construct_table trace (tex, fout_temp) bot_Env), EffBot) in
                update false n new_te m1'
            in
            (* let t1 = if x = "_" then find n1 m1' else replace_V (find n1 m1') x var in
            let prop_t = Table (construct_table cs (tx, t1)) in
            let px_t, t1 = prop_scope env1 env' x m1' prop_t t in
            let nf_t2_tf'_opt =
              Opt.map (fun (_, (nf, bf)) ->
                let envf, lf, fcs = get_vnode nf in
                let nf = construct_snode x nf in
                let tf = find nf m1' in
                let t2, tf' = prop_scope env' envf x m1' t tf in
                nf, t2, tf') f_nf_opt
            in
            let tx', t1' = io_T cs px_t in
            let m1' = m1' |> update nx tx' |> update n1 (if x = "_" then t1' else replace_V t1' z x) |>
            (Opt.map (fun (nf, t2, tf') -> fun m' -> m' |> update nf tf' |> update n (join_V t1 t2))
              nf_t2_tf'_opt |> Opt.get_or_else (update n t1)) in *)
            let m'' = join_M m1' m' in
            (* (if !debug && lx = "97" then 
              Format.fprintf Format.std_formatter "Line 1370 %a" pr_exec_map m1'
            );  *)
            m''
          end
      ) te (m |> update false n te |> Hashtbl.copy), [create_empty_trace]
      (* in
      (if !debug && l = "30" then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 1124, Lambda[l=49](prop-post), trace: %s, @,te: @[%a@]@."
           (get_trace_data trace)
           pr_value_and_eff (find n m'));
      m', tails' *)
  | MultiRec (f_opt, xlist, e1, _) ->
    (* failwith "Not implemented error" *)
    let n = loc term |> construct_enode env |> construct_snode trace in
    let te = find n m in
    
    let te =
      match te with
      | TEBot | _ when is_bot_VE te ->
        let tee =  init_T (append_call_trace (fresh_z ()) trace) (get_Relation ae) in
        Table tee |> init_VE_wec
      | _ -> temap (set_table_ae ae, fun _ -> Effect ec) te
    in
    
    let is_rec' = Opt.exist f_opt || is_rec in
    let add_trace = if is_rec' && List.for_all (fun (x, _) -> x = "_") xlist then false else true in          
    let f_nf_opt =
      Opt.map (fun (f, lf) -> f, (construct_vnode env lf trace, true)) f_opt
    in
    let env' =
      env |>
      (Opt.map (uncurry VarMap.add) f_nf_opt |>
        Opt.get_or_else (fun env -> env))
    in
    let prop_te =
      let rec make_prop_te xlist te env ae ec replace_fn =
        match xlist with
        | [] -> failwith "Empty xlist"
        | (x,lx)::xlist ->
          match te with
          | TypeAndEff (Table table, _) ->
            let (table, _) = get_full_table_T table in
            let prop_table = 
              List.fold_left (fun table_acc t_entry ->
                  let cs = fst t_entry in
                  let z = get_trace_data cs in
                  let tel = snd t_entry |> fst in
                  let fout = snd t_entry |> snd in

                  let nx = construct_vnode env lx cs in
                  let nx' = construct_snode cs nx in
                  let tex = find nx' m in
                  if is_bot_VE tex then table_acc else

                    let env' =
                      env |> VarMap.add x (nx, false) |> 
                      (if io_effects () then add_input_acc_to_env x nx
                      else id) |> 
                      if is_tuple_VE tel then
                        add_tuple_to_env x tel lx cs
                      else id
                    in

                    let tex = replace_fn tex in
                    let ae'' = get_ae_from_v (extract_v tex) in
                    let ae' = if (x <> "_" && is_Relation (extract_v tex)) || is_tuple_VE tex || is_List (extract_v tex) then 
                      (arrow_V z ae (extract_v tex)) else if only_shape_V ae'' then ae else stren_V ae ae'' 
                    in
                    let ec' = 
                      extract_eff tex
                      |> fun e -> stren_Eff e ae'
                      |> get_effmap
                      |> if io_effects () then StateMap.mapi (set_input_types_i z) else id
                    in

                    let replace te =
                      let te = replace_fn te in
                      if x = "_" then te
                      else if is_tuple_VE tex then
                        get_tuple_list_VE tex |> List.length |> first_n |>
                        List.fold_left (fun v i ->
                          let z_i = z^"."^(string_of_int i) in
                          let x_i = x^"."^(string_of_int i) in
                          replace_VE v x_i z_i
                          ) te
                      else
                        replace_VE te x z
                      |> 
                        if List.length xlist = 0 then
                          fun ve -> 
                            if io_effects () then
                              List.combine (acc_vars_list x) (acc_vars_list z)
                              |> List.fold_left (fun ve (old_var, new_var) -> replace_VE ve old_var new_var) ve
                            else ve
                        else id
                    in

                    let fout = 
                      match xlist with
                      | [] ->
                        let trace' = if add_trace then cs else trace in
                        let tails = get_fout_tails fout in
                        List.fold_left (fun fout tail ->
                            let trace' = extend_trace tail trace' in
                            let n1' = loc e1 |> construct_enode env' |> construct_snode trace' in
                            let te = find n1' m |> replace in
                            (* (if !debug && (loc term) = "92" then
                              let pr = Format.fprintf Format.std_formatter in
                              pr "@.LINE 1934, Fun, cs_trace: %s,@,te_temp: @[%a@]@."
                                (get_trace_data trace)
                                pr_value_and_eff te
                            ); *)
                            update_fout tail te fout
                          ) init_fout tails
                      | _ -> 
                        let _, ter = get_full_fout fout |> List.hd in
                        let ter =
                          match ter with
                          | TEBot | TypeAndEff (Bot, _) -> 
                            let table_t = 
                              init_T (append_call_trace (fresh_z ()) trace) (get_Relation ae')
                            in
                            TypeAndEff (Table (table_t), Effect ec')
                          | _ -> ter
                        in
                        (if !debug then
                          let pr = Format.fprintf Format.std_formatter in
                          pr "@.LINE 1989, Fun, cs_trace: %s,@,te_temp: @[%a@]@."
                            (get_trace_data trace)
                            pr_value_and_eff ter
                        );
                        let ve = make_prop_te xlist ter env' ae' ec' replace in
                        update_fout create_empty_trace ve init_fout
                    in
                    update_table cs (tex, fout) table_acc

                ) (init_T (dx_T te) (get_Relation ae)) table
            in
            TypeAndEff (Table prop_table, Effect ec)
          | _ -> failwith ("Expected table")
      in
      make_prop_te xlist te env' ae ec id
    in

    (if !debug then
      let pr = Format.fprintf Format.std_formatter in
      pr "@.LINE 1934, Fun, cs_trace: %s,@,te1: @[%a@]@,te_temp: @[%a@]@."
        (get_trace_data trace)
        pr_value_and_eff prop_te
        pr_value_and_eff te
    );
    let px_te, te1 = prop prop_te te in
    let nf_t2_tf'_opt =
      Opt.map (fun (_, (nf, _)) ->
        let nf = construct_snode trace nf in
        let tef = find nf m in
        (if !debug then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 1934, Fun, cs_trace: %s,@,te1: @[%a@]@,te_temp: @[%a@]@."
            (get_trace_data trace)
            pr_value_and_eff te1
            pr_value_and_eff tef
        );
        let te2, tef' = prop te1 tef in
        (if !debug then
          let pr = Format.fprintf Format.std_formatter in
          pr "@.LINE 1934, Fun, cs_trace: %s,@,te1: @[%a@]@,te_temp: @[%a@]@."
            (get_trace_data trace)
            pr_value_and_eff te2
            pr_value_and_eff tef'
        );
        nf, te2, tef') f_nf_opt
    in

    let m =
      Opt.map (fun (nf, te2, tef') -> 
        update true nf tef' m
        |> update false n te2) nf_t2_tf'_opt
      |> Opt.get_or_else (update false n te1 m)
    in

    let rec update_and_step xlist te env m ae ec cs_acc =
      match xlist with
      | [] -> failwith "Empty xlist"
      | (x,lx)::xlist ->
        match te with
        | TypeAndEff (Table table, _) ->
          let (table, _) = get_full_table_T table in
          List.fold_left (fun m t_entry ->
              let cs = fst t_entry in
              let z = get_trace_data cs in
              let tel = snd t_entry |> fst in
              let fout = snd t_entry |> snd in

              let nx = construct_vnode env lx cs in
              let nx' = construct_snode cs nx in
              let ae = get_ae_from_ve tel in
              let ec' = extract_ec tel in
              if is_bot_VE tel then m else
                let m = update is_rec' nx' tel m in

                let env' =
                  env |> VarMap.add x (nx, false) |> 
                  (if io_effects () then add_input_acc_to_env x nx
                  else id) |> 
                  if is_tuple_VE tel then
                    add_tuple_to_env x tel lx cs
                  else id
                in

                let fout' =
                  if x = "_" then fout
                  else if is_tuple_VE tel then
                    get_tuple_list_VE tel |> List.length |> first_n |>
                    List.fold_left (fun fout i ->
                      let z_i = z^"."^(string_of_int i) in
                      let x_i = x^"."^(string_of_int i) in
                      replace_fout replace_VE fout z_i x_i
                      ) fout
                  else
                    replace_fout replace_VE fout z x
                  |> 
                    if List.length xlist = 0 then
                      fun fout -> 
                        if io_effects () then
                          List.combine (acc_vars_list z) (acc_vars_list x)
                          |> List.fold_left (fun fout (old_var, new_var) -> replace_fout replace_VE fout old_var new_var) fout
                        else fout
                    else id
                in
                let ae'' = get_ae_from_v (extract_v tel) in
                let ae' = if (x <> "_" && is_Relation (extract_v tel)) || is_tuple_VE tel || is_List (extract_v tel) then 
                  (* if only_shape_V tx then ae else  *)
                  (arrow_V x ae (extract_v tel)) else if only_shape_V ae'' then ae else stren_V ae ae'' (* TODO: try only ae'' *)  
                in
                  (* (if !debug then
                    let pr = Format.fprintf Format.std_formatter in
                    pr "@.LINE 1934, Fun, cs_trace: %s,@,te1: @[%a@]@."
                      (get_trace_data trace)
                      pr_eff_map ec'
                  ); *)
                let ec' = (
                    if is_tuple_VE tel then
                      get_tuple_list_VE tel |> List.length |> first_n |>
                      List.fold_left (fun ec i ->
                        let z_i = z^"."^(string_of_int i) in
                        let x_i = x^"."^(string_of_int i) in
                        replace_Eff ec z_i x_i
                        ) (Effect ec')
                    else
                      replace_Eff (Effect ec') z x
                  )
                  |> fun e -> stren_Eff e ae'
                  |> get_effmap
                  |> if io_effects () then StateMap.mapi (set_input_types_i x) else id
                in
                  (* (if !debug then
                    let pr = Format.fprintf Format.std_formatter in
                    pr "@.LINE 1934, Fun, cs_trace: %s,@,te1: @[%a@]@."
                      (get_trace_data trace)
                      pr_eff_map ec'
                  ); *)

                match xlist with
                | [] ->
                  let trace' = if add_trace then cs else trace in
                  let tails = get_fout_tails fout' in
                  let m = List.fold_left (fun m tail ->
                        let trace' = extend_trace tail trace' in
                        let n1' = loc e1 |> construct_enode env' |> construct_snode trace' in
                        let te = v_fout tail fout' in
                        update false n1' te m
                      ) m tails
                  in
                  (* (if !debug then
                    let pr = Format.fprintf Format.std_formatter in
                    pr "@.LINE 1934, Fun, cs_trace: %s,@,te1: @[%a@]@."
                      (get_trace_data trace)
                      pr_eff_map ec'
                  ); *)
                  let m, tails = step e1 env' trace' ec' ae' assertion is_rec' m prev_m in
                  
                  if is_rec' then
                    let n1' = loc e1 |> construct_enode env' |> construct_snode trace' in
                    let body_val = List.fold_left 
                      (fun acc_val tail ->
                        let trace = extend_trace tail trace' in
                        let node = loc e1 |> construct_enode env' |> construct_snode trace in
                        let node_val = find node m in
                        join_VE acc_val node_val
                      ) TEBot tails in
                    update false n1' body_val m
                  else
                    let te = find n m in
                    let cs_acc = List.rev (cs::cs_acc) in
                    let temp_te = 
                      let rec make_temp_te te cs_acc =
                        match cs_acc with
                        | [] -> failwith "Empty cs_acc"
                        | [cs] ->
                          let tex,  _ = io_T cs te in
                          let fout_temp = List.fold_left
                            (fun fout' tail ->
                              update_fout tail TEBot fout'
                            ) init_fout tails in
                          TypeAndEff (Table (construct_table cs (tex, fout_temp) bot_Env), EffBot)
                        | cs::cs_acc ->
                          let tex, fout = io_T cs te in
                          let (_, te) = get_full_fout fout |> List.hd in
                          let sub_te = make_temp_te te cs_acc in
                          let fout_temp = update_fout create_empty_trace sub_te init_fout in
                          TypeAndEff (Table (construct_table cs (tex, fout_temp) bot_Env), EffBot)
                      in
                      make_temp_te te cs_acc
                    in
                    update false n temp_te m
                | _ ->
                  let _, ter = get_full_fout fout' |> List.hd in
                  update_and_step xlist ter env' m ae ec' (cs :: cs_acc)
            ) m table
        | TEBot -> m
        | _ -> failwith ("Expected table/bot")
    in
    update_and_step xlist px_te env' (hashtblcopy m) ae ec [], [create_empty_trace]

  | TupleLst (termlst,  _) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Tuple ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end); *)
      let n = loc term |> construct_enode env |> construct_snode trace in
      let te = find n m in 
      let te = match te with 
          | TEBot | _ when is_bot_VE te -> init_VE_wec (List.length termlst |> init_Tuple)
          | _ -> te
      in
      let tlist = get_tuple_list_VE te in
      (* (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 1160, Tuple, trace: %s, @,ec: @[%a@]@, @,te: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec pr_value_and_eff te); *)
      if List.length termlst = 0 then
        let te' = 
          let cte = init_VE_v (get_unit_from_v ae) in
          join_VE te cte in
        m |> update false n te', [create_empty_trace]
      else
        let tp, m',  _, ec' = List.fold_right 
          (fun (te, e) (tep', m, ae, ec) -> 
            let m', tails = step e env trace ec ae assertion is_rec m prev_m in
            let tee = merge_traces e tails m' in
            let ae' = get_ae_from_ve tee in
            let ne = loc e |> construct_enode env |> construct_snode trace in
            let tee', _ = prop tee te in
            let m'' = update false ne tee' m' in
            let te' = add_tuple_item_V tep' tee' in
            let ec' = extract_ec tee in
            (* let ec' = stren_Eff (Effect ec') ae |> get_effmap in *)
            te',  m'', ae', ec'
          ) (zip_list tlist termlst) (Tuple [], m, ae, ec) in
        let tep = TypeAndEff (tp, Effect ec') in
        m' |> update false n tep, [create_empty_trace]
  | PatMat (e, patlst,  _) ->
      (* (if !debug then
      begin
          Format.printf "\n<=== Pattern Match ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
      end); *)
      (* (if !debug then
         let pr = Format.fprintf Format.std_formatter in
         pr "@.LINE 1194, PatMat, trace: %s, @,ec: @[%a@]@."
           (get_trace_data trace) pr_eff_map ec); *)

      (* let ex = get_var_name e in *)
      let m0, tails0 = step e env trace ec ae assertion is_rec m prev_m in
      let map, tails = List.fold_left_map (fun m0' tail0 ->
        let trace0 = extend_trace tail0 trace in
        let ne = loc e |> construct_enode env |> construct_snode trace0 in
        let tee = find ne m0' in
        let ae0 = get_ae_from_ve tee in
        let ec0 = extract_ec tee in
        let ec0 = stren_Eff (Effect ec0) ae0 |> get_effmap in (* TODO: try removing this line *)
        let m0' = update false ne tee m0' in
        (* (if !debug && (get_label_snode ne = "EN: 95;(z18*)") then 
          Format.fprintf Format.std_formatter "Line 1370 %a" pr_exec_map m'
        ); *)
        if only_shape_VE tee then m0', [tail0]
        else 
          let m2, tails2 = 
          (* (if !debug && (loc e) = "131" then 
            begin
              let pr = Format.fprintf Format.std_formatter in
              pr "\nLOC 131:@,tee:@[%a@]" pr_value_and_eff tee
            end); *)
          List.fold_left 
            (fun (m_acc, tails_acc) (Case (e1, e2)) -> 
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
              | Const  _ ->
                let m1, _ = step e1 env trace0 ec0 ae0 assertion is_rec m_acc prev_m in
                let n1 = loc e1 |> construct_enode env |> construct_snode trace0 in
                let te1 = find n1 m1 in
                let m1 = update false n1 te1 m1 in
                let tee, te1 = find ne m1, find n1 m1 in
                let tee, te1 = alpha_rename_VEs tee te1 in
                let te1 = temap (id, fun _ -> extract_eff tee) te1 in
                if leq_VE tee te1 then m_acc, tails_acc else
                  let te1 = if is_List (extract_v tee) then
                              item_shape_VE tee te1
                            else te1 in
                  (* (if true then
                      begin
                      Format.printf "\n Pattern %s\n" (loc e1);
                      pr_exp true Format.std_formatter e1;
                      Format.printf "\n";
                      pr_value Format.std_formatter t1;
                      Format.printf "\n<<~~~~>> %s\n" (loc e);
                      pr_value Format.std_formatter te;
                      Format.printf "\n";
                      pr_value Format.std_formatter (join_V t1 te);
                      Format.printf "\n";
                      end
                      ); *)
                  let m1 = m1 |> update false n1 te1 in
                  let b = sat_leq_VE te1 tee in
                  let n1 = loc e1 |> construct_enode env |> construct_snode trace0 in
                  (* (if true then
                      begin
                      Format.printf "\npattern ae: %b\n" b;
                      pr_value Format.std_formatter ae;
                      Format.printf "\n";
                      end
                      ); *)
                  let ae1 = if not b then bot_relation_V Int else 
                              arrow_V (loc e) ae0 (extract_v (find n1 m1)) in
                  (* (if true then
                      begin
                      Format.printf "\nRES for ae:\n";
                      pr_value Format.std_formatter ae';
                      Format.printf "\n";
                      end
                      );  *)
                  (* let ec'' = extract_ec te1 in *)
                  let ec2 = extract_ec te1 
                            |> (fun ec'' ->  arrow_EffV (loc e) (Effect ec'') 
                                            (extract_v (find n1 m1)))
                            |> (function Effect ec'' -> ec'' | EffBot | EffTop -> StateMap.empty) in
                  let m2, tails2 = step e2 env trace0 ec2 ae1 assertion is_rec m1 prev_m in
                  let map, tails = List.fold_left_map (fun m2' tail2 ->
                    let extended_tail = extend_trace tail2 tail0 in
                    let extended_tail' = 
                      if List.length patlst > 1 && !if_part && trace_isempty tail2 then
                        append_part_trace (create_pat_token (loc term) (loc e1)) extended_tail 
                      else extended_tail 
                    in
                    let trace2 = extend_trace tail2 trace0 in
                    let trace = extend_trace extended_tail' trace in
                    let n2 = loc e2 |> construct_enode env |> construct_snode trace2 in
                    let n = loc term |> construct_enode env |> construct_snode trace in
                    let te2 = find n2 m2' in
                    let m2' = update false n2 te2 m2' in
                    if not b then 
                      let te, te2 = find n m2', find n2 m2' in
                      let te2', te' = prop te2 te in
                      m2' |> update false n1 te1 |> update false n2 te2' |> update false n te', (extended_tail')
                    else
                      let tee, te, te1, te2 = find ne m2', find n m2', find n1 m2', find n2 m2' in
                      let te1', tee' = let tee, te1 = alpha_rename_VEs tee te1 in 
                                        te1, join_VE te1 tee 
                      in
                      let te2', te' = prop te2 te in
                      m2' |> update false ne tee' |> update false n1 te1' 
                      |> update false n2 te2' |> update false n te', (extended_tail')
                  ) m2 tails2 in
                  map, tails_acc @ tails
              | Var (x, l') ->
                let n1 = trace0 |> construct_vnode env l' in
                let env1 = env |> VarMap.add x (n1, false) in
                let n1 = construct_snode trace0 n1 in
                let te1 = find n1 m_acc in 
                let tee = find ne m_acc in
                let _, te1' = prop tee te1 in
                let m1 = m_acc |> update false n1 te1' in
                (* let ec' = extract_ec te1' in *)
                let ec1 = extract_ec te1' 
                          |> (fun ec' -> if (x <> "_" && is_Relation (extract_v te1')) || 
                                          is_List (extract_v te1') then 
                                        (arrow_EffV x (Effect ec') (extract_v te1')) 
                                      else Effect ec')
                          |> (function Effect ec' -> ec' | EffBot | EffTop -> StateMap.empty) in
                let m2, tails2 = step e2 env1 trace0 ec1 ae0 assertion is_rec m1 prev_m in
                let map, tails = List.fold_left_map (fun m2' tail2 ->
                  let extended_tail = extend_trace tail2 tail0 in
                  let extended_tail' = 
                    if List.length patlst > 1 && !if_part && trace_isempty tail2 then
                      append_part_trace (create_pat_token (loc term) (loc e1)) extended_tail 
                    else extended_tail 
                  in
                  let trace2 = extend_trace tail2 trace0 in
                  let trace = extend_trace extended_tail' trace in
                  let n2 = loc e2 |> construct_enode env1 |> construct_snode trace2 in
                  let n = loc term |> construct_enode env |> construct_snode trace in
                  let te2 = find n2 m2' in
                  let m2' = update false n2 te2 m2' in
                  let te = find n m2' in 
                  let te2 = find n2 m2' in
                  let te2', te' = prop te2 te in
                  m2' |> update false n2 te2' |> update false n te', extended_tail'
                ) m2 tails2 in
                map, tails_acc @ tails
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
                let ml, envl = list_var_item el trace0 m_acc env ne true 0 in
                (* (if !debug then
                    begin
                    Format.printf "\n<=== Pattern binop ===>\n";
                    pr_exp true Format.std_formatter er;
                    Format.printf "\n";
                    end
                    ); *)
                let mr, envr = list_var_item er trace0 ml envl ne false 1 in
                let nr = loc er |> construct_enode envr |> construct_snode trace0 in
                let n1 = l' |> construct_enode envr |> construct_snode trace0 in
                let m1, _ = step e1 envr trace0 ec0 ae0 assertion is_rec mr prev_m in
                let te1 = find n1 m1 in
                let m1 = update false n1 te1 m1 in
                let tee, te1 = find ne m1, find n1 m1 in
                let  _, te1 = alpha_rename_VEs tee te1 in
                let tenr = find nr m1 in
                let ae1 =
                  let ae = arrow_V (loc e) ae0 (extract_v te1) in 
                  arrow_V (loc er) ae (extract_v tenr)
                in
                (* let ec' = extract_ec te1 in *)
                let ec1 = extract_ec te1 
                          |> (fun ec' -> if (is_Relation (extract_v te1)) || 
                                      is_List (extract_v te1) then 
                                        (arrow_EffV (loc e) (Effect ec') (extract_v te1)) 
                                      else Effect ec')
                          |> (function Effect ec' -> ec' | EffBot | EffTop -> StateMap.empty) in
                
                let ec1 = ec1 
                          |> (fun ec' -> if is_List (extract_v tenr) then 
                                        arrow_EffV (loc er) (Effect ec') (extract_v tenr)
                                      else Effect ec') 
                          |> (function Effect ec' -> ec' | EffBot | EffTop -> StateMap.empty) in
                let m2, tails2 = step e2 envr trace0 ec1 ae1 assertion is_rec m1 prev_m in
                let map, tails = List.fold_left_map (fun _ tail2 ->
                  let extended_tail = extend_trace tail2 tail0 in
                  let extended_tail' = 
                    if List.length patlst > 1 && !if_part && trace_isempty tail2 then
                      append_part_trace (create_pat_token (loc term) (loc e1)) extended_tail 
                    else extended_tail 
                  in
                  let trace2 = extend_trace tail2 trace0 in
                  let trace = extend_trace extended_tail' trace in
                  let n2 = loc e2 |> construct_enode envr |> construct_snode trace2 in
                  let n = loc term |> construct_enode env |> construct_snode trace in
                  let te2 = find n2 m2 in
                  let m2 = update false n2 te2 m2 in
                  let tee, te, te1, te2 = find ne m2, find n m2, find n1 m2, find n2 m2 in
                  let te1', tee' =
                    let tee, te1 = alpha_rename_VEs tee te1 in 
                    prop te1 tee
                  in
                  let te2', te' = prop te2 te in
                  m2 |> update false ne tee' |> update false n1 te1'
                  |> update false n2 te2' |> update false n te', extended_tail'
                ) m2 tails2 in
                map, tails_acc @ tails
              | TupleLst (termlst, l') ->
                let n1 = l' |> construct_enode env |> construct_snode trace0 in
                let te1 = 
                  let raw_te1 = find n1 m_acc in
                  if is_tuple_VE raw_te1 then raw_te1
                  else let u' = List.init (List.length termlst) (fun _ -> TEBot) in 
                        init_VE_wec (Tuple u') 
                in 
                let tee = find ne m_acc in
                let tee, te1 = alpha_rename_VEs tee te1 in
                let tlst = get_tuple_list_VE te1 in
                let tllst = get_tuple_list_VE tee in
                (* (if !debug then 
                  begin 
                    let pr = Format.fprintf Format.std_formatter in 
                    pr "LINE 1568, PatMat.TupleLst, te1: @[%a@]@, tee: @[%a@]@." 
                    pr_value_and_eff te1 pr_value_and_eff tee;
                  end);  *)
                let env1, ae1, ec1, m1, tlst', tllst' = 
                  List.fold_left2 (fun (env1', ae1', ec1', m1', li, llst) e (tei, telsti) -> 
                      match e with
                      | Var (x, l') -> 
                          let nx = construct_vnode env l' trace0 in
                          let env1' = env1' |> VarMap.add x (nx, false) in
                          let nx = construct_snode trace0 nx in
                          let tex = find nx m1' in
                          let telsti', tei' = prop telsti tei in
                          let tei'', tex' = prop tei' tex in
                          (* (if !debug && get_label_snode nx = "VN: 137;z25.z132.." then 
                            begin 
                              let pr = Format.fprintf Format.std_formatter in 
                              pr "LINE 1702, PatMat.TupleLst, telsti: @[%a@]@, tei: @[%a@]@, tex: @[%a@]@." 
                              pr_value_and_eff telsti'
                              pr_value_and_eff tei''
                              pr_value_and_eff tex'
                          end);  *)
                          let m1' = m1' |> update false nx tex' in
                          let ae1', ec1' = if (x <> "_" && is_Relation (extract_v tex')) || is_tuple_VE tex' || is_List (extract_v tex') then 
                            (* if only_shape_V tx then ae else  *)
                            (arrow_V x ae1' (extract_v tex'), arrow_EffV x ec1' (extract_v tex')) else ae1', ec1' in
                          (* (if !debug && get_label_snode nx = "VN: 137;z25.z132.." then 
                            begin 
                              let pr = Format.fprintf Format.std_formatter in 
                              pr "LINE 1702, PatMat.TupleLst, telsti: @[%a@]@, tei: @[%a@]@, tex: @[%a@]@." 
                              pr_value_and_eff telsti'
                              pr_value_and_eff tei''
                              pr_value_and_eff (find nx m1')
                          end);  *)
                          env1', ae1', ec1', m1', tei'' :: li, telsti' :: llst
                      | _ -> begin 
                          (* (if !debug then 
                            begin 
                              let pr = Format.fprintf Format.std_formatter in 
                              pr "LINE 1269, PatMat.TupleLst, e: @[%a@]@." (pr_exp true) e
                            end);  *)
                          raise (Invalid_argument "Tuple only for variables now")
                        end
                    ) (env, ae0, Effect ec0, m_acc, [], []) termlst (zip_list tlst tllst) in
                let tlst', tllst' = List.rev tlst', List.rev tllst' in
                let te1', tee' = TypeAndEff (Tuple tlst', ec1), 
                                  TypeAndEff (Tuple tllst', (extract_eff tee)) 
                in
                (* (if !debug then 
                  begin 
                    let pr = Format.fprintf Format.std_formatter in 
                    pr "LINE 1568, PatMat.TupleLst, node: @[%s@]@, ae: @[%a@]@." 
                    (get_label_snode n1) pr_value ae1;
                  end);  *)
                (* (if !debug then 
                  begin 
                    let pr = Format.fprintf Format.std_formatter in 
                    pr "LINE 1568, PatMat.TupleLst, node: @[%s@]@, te1: @[%a@]@, tee: @[%a@]@, te1': @[%a@]@, tee': @[%a@]@." 
                    (get_label_snode n1) pr_value_and_eff te1 pr_value_and_eff tee pr_value_and_eff te1' pr_value_and_eff tee'
                  end);  *)
                let tee', _ = prop tee tee' in
                (* (if !debug then 
                  begin 
                    let pr = Format.fprintf Format.std_formatter in 
                    pr "LINE 1579, PatMat.TupleLst, node: @[%s@]@, te1': @[%a@]@, tee': @[%a@]@." 
                    (get_label_snode n1) pr_value_and_eff te1' pr_value_and_eff tee'
                  end);  *)
                let m1 = m1 |> update false n1 te1' |> update false ne tee' in
                let m2, tails2 = step e2 env1 trace0 (get_effmap ec1) ae1 assertion is_rec m1 prev_m in
                let map, tails = List.fold_left_map (fun m2' tail2 ->
                  let extended_tail = extend_trace tail2 tail0 in
                  let extended_tail' = 
                    if List.length patlst > 1 && !if_part && trace_isempty tail2 then
                      append_part_trace (create_pat_token (loc term) (loc e1)) extended_tail 
                    else extended_tail 
                  in
                  let trace2 = extend_trace tail2 trace0 in
                  let trace = extend_trace extended_tail' trace in
                  let n2 = loc e2 |> construct_enode env1 |> construct_snode trace2 in
                  let n = loc term |> construct_enode env |> construct_snode trace in
                  let te = find n m2' in 
                  let te2 = find n2 m2' in
                  let te2', te' = prop_scope env1 env m2' te2 te in
                  (* (if !debug && loc term = "144" then 
                    Format.fprintf Format.std_formatter "Line 1698 %s %s %a" 
                  (get_label_snode n) (get_label_snode n2) pr_value_and_eff te'); *)
                  m2' |> update false n2 te2' |> update false n te', extended_tail'
                ) m2 tails2 in
                map, tails_acc @ tails
              | _ -> raise (Invalid_argument "Pattern should only be either constant, variable, or list cons")
            ) (update false ne tee m0' |> Hashtbl.copy, []) patlst in
          (* (if !debug && (get_label_snode ne = "EN: 86;(z18*)") then 
            pr_exec_map Format.std_formatter m''
          ); *)
          m2, tails2
      ) m0 tails0
      in map, List.flatten tails |> List.sort_uniq (fun tail1 tail2 -> comp_trace tail1 tail2)
  | Event (e1,  _) ->
    (* (if !debug then
      begin
        Format.printf "\n<=== EV ===>\n";
        pr_exp true Format.std_formatter term;
        Format.printf "\n";
      end); *)
      let m1, tails1 = step e1 env trace ec ae assertion is_rec m prev_m in
      List.fold_left 
        (fun m tail1 ->
          let trace = extend_trace tail1 trace in
          let n = loc term |> construct_enode env |> construct_snode trace 
                  |> (fun n -> (if assertion then add_ev_asst (loc term) (n, env, trace) else ()); n) in
          let n1 = loc e1 |> construct_enode env |> construct_snode trace in
          let te1 = find n1 m1 in
          if is_bot_VE te1 then m else
          let prev_te1 = find n1 prev_m in
          if leq_VE te1 prev_te1 then m else
          let ae1 = get_ae_from_ve te1 in
          let penv = get_env_list env m1 in
          let te' = begin match te1 with
                    | TypeAndEff ((Relation v), (Effect e)) -> 
                      TypeAndEff (get_unit_from_v ae1, Effect (ev penv e v))
                    | _ -> te1
                    end
          in 
          (* (if !debug then
            let pr = Format.fprintf Format.std_formatter in
            pr "@.LINE 1471, @,te': @[%a@]@."
              pr_value_and_eff te'); *)
          m |> update false n (stren_VE te' ae1)
        ) m1 tails1, tails1
  | Assert (e1,  _,  _) ->
     (* (if !debug then
        begin
          Format.printf "\n<=== ASSERT ===>\n";
          pr_exp true Format.std_formatter term;
          Format.printf "\n";
        end); *)
      let m1, tails1 = step e1 env trace ec ae assertion is_rec m prev_m in
      List.fold_left 
        (fun m tail1 ->
          let trace = extend_trace tail1 trace in
          let n = loc term |> construct_enode env |> construct_snode trace in
          let n1 = loc e1 |> construct_enode env |> construct_snode trace in
          let te1 = find n1 m in
          let ae1 = get_ae_from_ve te1 in
          let ec' = extract_eff te1 in
          if assertion then add_reg_asst (loc term) ae1 n1;
          let te' = TypeAndEff (get_unit_from_v ae1, ec') in
          m |> update false n te'
        ) m1 tails1, tails1
  in
  (if !debug then Format.fprintf Format.std_formatter "%s_%s end\n" (loc term) (get_trace_data trace));
  (* (if !debug then Format.fprintf Format.std_formatter "line 1837 %a" pr_exec_map m); *)
  m, tails
  end
         
     

let step x1 x2 x3 x4 x5 x6 x7 x8 = measure_call "step" (step x1 x2 x3 x4 x5 x6 x7 x8)
          
(** Widening **)
let widening k (m1:exec_map_t) (m2:exec_map_t): exec_map_t =
  if k > !delay_wid then wid_M m1 m2 else join_M m1 m2

let widening k m = measure_call "widening" (widening k m)

    
(** Narrowing **)
let narrowing (m1:exec_map_t) (m2:exec_map_t): exec_map_t =
  meet_M m1 m2 

(** Check assertion **)
type failed_asst_t = RegularAsst of loc * pos | PropEvAsst of loc | PropFinalAsst
let add_failed_assertion (fasst: failed_asst_t) fassts = fasst::fassts

let rec check_assert term m fassts = 
  let find n m = NodeMap.find_opt n m |> Opt.get_or_else TEBot in
  let envOfE env trace = 
    VarMap.fold (fun x (nx, _) envE -> 
        let nx = construct_snode trace nx in
        match (find nx m |> extract_v) with
        | Relation r -> VarMap.add x r envE
        | _ -> envE) env (VarMap.empty)
  in
  match term with
  | Const _ | Var _ | NonDet _ -> fassts
  | BinOp (_, e1, e2, _) ->
     check_assert e1 m fassts |> check_assert e2 m
  | UnOp (_, e1, _) ->
     check_assert e1 m fassts
  | Ite (e0, e1, e2, _) -> 
     check_assert e0 m fassts |> check_assert e1 m |> check_assert e2 m
  | App (e1, e2, _) ->
     check_assert e1 m fassts |> check_assert e2 m
  | MultiApp (e1, l_e2, _) ->
    List.fold_left (fun fassts (_, e) -> check_assert e m fassts) (check_assert e1 m fassts) l_e2
  | Rec (_, _, e1, _) ->
     check_assert e1 m fassts
  | MultiRec (_, _, e1, _) ->
     check_assert e1 m fassts
  | TupleLst (tlst, _) ->
     List.fold_right (fun e fs -> check_assert e m fs) tlst fassts
  | PatMat (e, patlst, _) -> 
     check_assert e m fassts
     |> List.fold_right (fun (Case (e1, e2)) fs -> check_assert e1 m fs |> check_assert e2 m) patlst
  | Event _ ->
     let l = loc term in
     let report_fasst fs = function
       | true -> fs
       | false -> add_failed_assertion (PropEvAsst l) fs
     in
     AsstsMap.find_opt l !evassts 
     |> Opt.map (fun ns -> 
            List.fold_right (fun (n, env, trace) fs ->
                let te = find n m in
                let envE = envOfE env trace in
                match (extract_eff te) with
                (* | EffBot -> AbstractEv.check_assert_bot (EvAssert l) |> report_fasst fs *)
                | Effect eff -> AbstractEv.check_assert (EvAssert l) envE eff |> report_fasst fs
                (* | EffTop -> AbstractEv.check_assert_top (EvAssert l) |> report_fasst fs *)
                | EffBot | EffTop -> report_fasst fs true
                ) ns fassts)
     |> Opt.get_or_else fassts
  | Assert (e1, pos,  _) ->
     let l = loc term in
     AsstsMap.find_opt l !regassts 
     |> Opt.map (fun ns ->  
            List.fold_right (fun (n, ae) fs ->
                let te = find n m in
                let t = extract_v te in
                if not (is_bool_bot_V t) && not (is_bool_false_V t) && not (only_shape_V ae) then
                  begin
                    (if !debug then 
                       begin
                          Format.fprintf Format.std_formatter 
                            "\nASSERTION CHECKING (regular) @@LOC %s ***FAILED***:@,@[<v>@[n:%a@]@,@[te:%a@]@]@."
                            (loc term) pr_node n pr_value_and_eff te
                       end);
                    add_failed_assertion (RegularAsst (loc term, pos)) fs
                  end
                else if (is_bool_bot_V t) && (is_asst_false e1 = false && only_shape_V ae = false) then
                  begin
                    (if !debug then 
                       begin 
                         Format.fprintf Format.std_formatter 
                           "\nASSERTION CHECKING (regular) @@LOC %s ***FAILED***:@,@[<v>@[n:%a@]@,@[te:%a@]@]@."
                           (loc term) pr_node n pr_value_and_eff te
                       end);
                    add_failed_assertion (RegularAsst (loc term, pos)) fs
                  end 
                else
                  begin
                    (if !debug then 
                       begin 
                         Format.fprintf Format.std_formatter 
                           "\nASSERTION CHECKING (regular) @@LOC %s ===VALID===:@,@[<v>@[n:%a@]@,@[te:%a@]@]@."
                           (loc term) pr_node n pr_value_and_eff te
                       end);
                    fs
                  end
              ) ns fassts)
     |> (function 
         | None -> 
            ((if !debug then 
               begin 
                 Format.fprintf Format.std_formatter "\nASSERTION CHECHING (regular) @@LOC %s: (vacuously holds)@."
                   (loc term)
               end); fassts)
         | Some fs -> fs)

     |> check_assert e1 m

let check_assert_final_eff te env m fassts =
  let find_v n m = NodeMap.find_opt n m |> Opt.get_or_else TEBot |> extract_v in
  let report_fasst fs = function 
    | true -> fs
    | false -> add_failed_assertion PropFinalAsst fs
  in 
  let envE = VarMap.fold (fun x (n_var, _) envE -> 
                 let s_var = construct_snode create_empty_trace n_var in
                 match find_v s_var m with
                 | Relation r -> VarMap.add x r envE
                 | _ -> envE
               ) env VarMap.empty
  in 
  match (extract_eff te) with
    | EffBot -> report_fasst fassts true
    | Effect eff -> AbstractEv.check_assert FinalAssert envE eff |> report_fasst fassts 
    | EffTop -> AbstractEv.check_assert_top (FinalAssert) |> report_fasst fassts 

let check_assert term m = measure_call "check_assert" (check_assert term m)

let check_assert_final_eff te env m = measure_call "check_assert_final" (check_assert_final_eff te env m)
 
(** Fixpoint loop **)
let rec fix stage env e (k: int) (m:exec_map_t) (assertion:bool): string * exec_map_t =
  (if !out_put_level = 0 then
    begin
      let process = match stage with
      | Widening -> "Wid"
      | Narrowing -> "Nar"
      in
      Format.printf "\n%s step %d\n" process k;
      print_exec_map m;
    end);
  (* if k > 40 then exit 0 else *)

  (* let eff_i = AbstractEv.eff0 () in *)
  let arrow_ec var ec v = match (arrow_EffV var (Effect ec) v) with Effect ec' -> ec' | _ -> ec in
  let ae, eff_i, pre_vars_vs = 
    VarMap.fold (fun var (n,  _) (ae, ec, pvs) ->
        let n = construct_snode create_empty_trace n in
        let find n m = NodeMap.find_opt n m |> Opt.get_or_else TEBot in
        let te = find n m in
        match (extract_v te) with
        | (Relation r) as v -> (arrow_V var ae v, arrow_ec var ec v, VarDefMap.add var r pvs) 
        | _ -> (ae, ec, pvs)
        (* if is_Relation (extract_v te) then *)
        (*   extract_v te |> (fun v ->  (arrow_V var ae v, arrow_ec var ec v, VarDefMap.add var v pvs)) *)
        (* else (ae, ec, pvs) *)
      ) env (Relation init_Env, AbstractEv.eff0 (), VarDefMap.empty) in
  let _ = AbstractEv.program_pre_vars := pre_vars_vs in
  let m_t = hashtblcopy m in
  (* (if !debug then
     let pr = Format.fprintf Format.std_formatter in
     pr "@.ae = @[%a@]" pr_value ae); *)

  (* let pp_eff e = 
    let ppf = Format.std_formatter in
    if StateMap.is_empty e then Format.fprintf ppf "Empty\n" 
    else StateMap.bindings e 
         |> Format.pp_print_list ~pp_sep: (fun ppf () -> Format.printf ";@ ") pr_eff_binding ppf in *)
  (* (if !debug then (Format.printf "\nEff0: "; pp_eff eff_i)); *)
  let m', tails = step e env create_empty_trace eff_i ae assertion false m_t m in
  (* (if !debug then check_map m'); *)
  if k < 0 then if k = -1 then "", m' else fix stage env e (k+1) m' assertion else
  (* if k > 2 then Hashtbl.reset !pre_m;
  pre_m := m; *)
  (* Format.printf "\nFinish step %d\n" k;
  flush stdout; *)
  let m'' = if stage = Widening then widening k m m' else narrowing m m' in
  (* (if !debug then Format.fprintf Format.std_formatter "%a" pr_exec_map m'); *)
  let comp = if stage = Widening then leq_M m'' m else leq_M m m'' in
  if comp then
    begin
      if assertion (* || !narrow *) then
        let fassts = List.fold_left (fun fassts tail -> 
          let prog_n = loc e |> construct_enode env |> construct_snode tail in
          let prog_te = NodeMap.find_opt prog_n m |> Opt.get_or_else TEBot in
          List.rev (check_assert e m [] |> check_assert_final_eff prog_te env m) @ fassts
          ) [] tails
        in
        (if !debug then 
           begin 
             Format.fprintf Format.std_formatter "\n# failed assertionsf: %d\n" (List.length fassts)
           end);
        let sr, sf = (List.fold_right 
                        (fun fasst (sr,sf) -> 
                          match fasst with
                          | RegularAsst (l, pos) ->
                             begin match List.assoc_opt l sr with
                             | None -> 
                                let s' =  try print_loc pos with
                                            Input_Assert_failure s -> s ^ " label " ^ l in 
                                ((l,s')::sr,sf)
                             | Some _ -> (sr,sf)
                             end
                          | PropEvAsst l -> 
                             begin match List.assoc_opt l sf with
                             | None -> 
                                let s' = "effect (post ev) assertion failed at label " ^ l in
                                (sr, (l,s')::sf)
                             | Some _ -> (sr, sf)
                             end
                          | PropFinalAsst ->
                             let s' = "effect (final) assertion failed" in
                             (sr, (loc e, s')::sf)
                        ) fassts ([],[]))
                     |> (fun (sr, sf) -> (List.map (fun (_, s) -> s) sr, List.map (fun (_,s) -> s) sf))
        in 
        let s = (String.concat ", " sr) ^ (String.concat ", " sf) in 
        let s = if (String.length s) = 0 then 
                  "The input program is safe.\n"  
                else "The program may not be safe: " ^ s ^ ".\n" in
        s, m
      else 
        fix stage env e k m true (* Final step to check assertions *)
    end
  else (fix stage env e (k+1) m'' assertion) (*Hashtbl.reset m; *)

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
          let n = construct_snode create_empty_trace n in
          (Hashtbl.remove m0' n; false)) envt
  in
  thresholdsSet := !thresholdsSet |> ThresholdsSetType.add 0 |> ThresholdsSetType.add 111 |> ThresholdsSetType.add 101
  |> ThresholdsSetType.add 2 |> ThresholdsSetType.add 4 |> ThresholdsSetType.add (-1) |> ThresholdsSetType.add (-2);
  (* pre_m := m0'; *)
  let e = 
    if !quick_prop then
      coalesce_app e 
      |> coalesce_rec
    else e
  in
  if !out_put_level < 2 then
              (print_endline "Executing:";
               print_exp stdout e);
  let check_str, m =
    let s1, m1 = (fix Widening envt e 0 m0' false) in
    if !narrow then
      begin
        narrow := false;
        (*let _, m1 = fix envt e (-10) m1 false in (* step^10(fixw) <= fixw *)*)
        let m1 = m1 |> reset in
        let s2, m2 = fix Narrowing envt e 0 m1 false in
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

let s = measure_call "s" (s)