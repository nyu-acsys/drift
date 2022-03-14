(* An Accumulating Automaton *)

(*
User:
   Q = {q0,...,}
   fun d q acc -> (acc,q')

2. Translations (--adt  --int)
    a. ADT encoding? Q0 | Q1...
    b. list of int: 0 | 1 | ...

3. output: problem.ml
    a. Mochi?
    b. Drift specialized abstract domain. finite value domain product with poly?

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
