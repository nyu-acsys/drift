(* An Accumulating Automaton *)

let rec xform_seq e1 e2 f (* v1 -> v2 -> ast *) =
  PatMat (xform e1 acc_e, [
    Case ( TupleList([acc'], v1), 
       PatMat (xform e2 acc', [
          Case ( TupleList([acc''], v2), 
            TupleList(
               [acc''; (f v1 v2)], 66666))
       ])
    )
  ])

let rec xform e acc_e =
  match e with
    | Const _ 
    | Var (x, loc) -> TupleList ([acc_e;e], 66666)
    | BinOp (bop, e1, e2, loc) ->
      xform_seq e1 e2 (fun v1 v2 -> (BinOp (bop, v1, v2, loc)))
    | App (e1, e2, loc) ->
      xform_seq e1 e2 (fun v1 v2 -> (App (v1, v2, loc)))

    | UnOp (_, e, _) -> fv bvs acc e
    | Ite (b, t, e, _, _) -> List.fold_left (fv bvs) acc [b; t; e]
    | PatMat (t, ps, _) ->
        let acc1 = fv bvs acc t in
        List.fold_left (fun acc (Case (p, t)) ->
          let bvs1 = fv StringSet.empty bvs p in
          fv bvs1 acc t)
          acc1 ps
    | TupleLst (ts, _) ->
        List.fold_left (fv bvs) acc ts
    | Rec (f_opt, (x, _), e, _) ->
        let d = StringSet.of_list (x :: (f_opt |> Opt.map fst |> Opt.to_list)) in
        fv (StringSet.union bvs d) acc e
  in

let product acc_e e = failwith "unimplemented"

type acc_t = int*int*int

type user_accaut = {
  q0 : int;
  qstates: int list;
  acc0: acc_t;
  trans: int -> acc_t -> int -> (int * acc_t); (* q a d -> q a *)
  final: int -> bool;
}

let user_accaut_of_program (fn:string) = 
  match fn with
  | "3states" -> {
      q0=0;
      qstates=[0;1;2;3];
      acc0=(0,0,0); (* unused *)
      trans=(fun q a d -> match (q,a) with 
      | (0,_) -> 1
      | (1,_) -> 2
      | (2,_) -> 3
      | (3,_) -> 3
      );
      final=(fun q -> q = 0)
    }
  | "order" -> { 
    (* assume c!=0. Property both c and -c cannot happen *)
      q0=0;
      qstates=[0;666];
      acc0=(0,0,0); (* neg c, pos c, _ *)
      trans=(fun q a d -> match (q,a) with 
      | (666,_) -> (666,a)
      | (0,(0,0,_)) -> 
           if d > 0 then (0,(0,d,0)) else (0,(d,0,0))
      | (0,(n,p,_)) ->
           if d * -1 == n then (666,a) 

      | (1,_) -> 2
      | (2,_) -> 3
      | (3,_) -> 3
      );
      final=(fun q -> q = 0)
    }
  | _ -> failwith "need user input"

(*
User:
   Q = {q0,...,}
   fun d q acc -> (acc,q')

2. Translations (--adt  --int)
    a. ADT encoding? Q0 | Q1...
    b. list of int: 0 | 1 | ...

3. output: problem.ml
    a. Mochi with --adt
    b. Mochi with --int
    c. Drift with --int
        specialized abstract domain. finite value domain product with poly?

type acc_aut_state = 
  | Q0 
  | Q1 of int
  | Q2 of bool * int
*)

module type AccAut =
  sig
    type acc_t
    type qs_t

    val to_term_init  : unit -> DriftSyntax.term
    val to_term_ev    : unit -> DriftSyntax.term
    val to_term_final : unit -> DriftSyntax.term
    val to_string : unit -> string
end

type last_qs = LastQ0

module LastAA = AccAut(struct 
  type acc_t = int (* save the last one *)
  type qs_t = last_qs
  let to_term_init () = 
    let l = {pl=666;pc=666} in 
       (* AccAut set the initial automaton state and accumulator value *)
       TupleLst [LastQ0;Const(Boolean(false),l);Const(Integer(1),l)]
  let to_term_ev () = 
    mk_lambda (Var("d",l)) (
       mk_lambda (Var("acc",l)) (
           (* AccAut Transition function here: *)
           (* let ev d (q,acc) = (q0,d) *)
           TupleLst [LastQ0;Const(Boolean(true),l);Var("d",l)] (* todo - make "d" from acc_t *)
       )
    )
  let to_term_final () = 
    mk_lambda (Var("acc",l)) (
        (* AccAut check if Accepting state or property of accumulator: *)
        App( (* assert *), BinOp(Gt,(Var("acc",l)),Const(Integer(5)),l))
    )
end)
