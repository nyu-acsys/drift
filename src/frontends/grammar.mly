%{
(* header *)
(** {0} Grammar rules for parser of Mini-OCaml *)

open Util
open Syntax
open Lexing

let mk_position s e =
  let start_pos = Parsing.rhs_start_pos s in
  { pos_line = start_pos.pos_lnum;
    pos_col = start_pos.pos_cnum - start_pos.pos_bol;
  }

%}
/* declarations */

%token <string> IDENT
%token <int> INTVAL
%token <bool> BOOLVAL
%token LPAREN RPAREN
%token PLUS MINUS DIV TIMES MOD
%token EQ NE LE GE LT GT
%token AND OR ARROW
%token IF ELSE THEN FUN LET REC IN
%token WHILE
%token EOF

%start main
%type <Syntax.term> main
%% /* rules */

main:
| term { $1 }
;

ident_list:
| IDENT ident_list { $1 :: $2 }
| IDENT { [$1] }
;

basic_term: (*term@7 := int | bool | var | (term)*)
| INTCONST { IntConst ($1, mk_position 1 1) }
| BOOLCONST { BoolConst ($1, mk_position 1 1) }
| IDENT { Var ($1, mk_position 1 1) }
| LPAREN term RPAREN { $2 } (*Parentheses*)
;

app_term: (* term@6 := term@6 term@7 | term@7 *)
| basic_term { $1 }
| app_term basic_term { App ($1, $2, mk_position 1 2) }
;

mult_op:
| TIMES { Mult }
| DIV { Div }
| MOD { Mod }
;

mul_term: (* term@5 := term@5 mop term@6 *)
| app_term { $1 }
| mul_term mult_op app_term { BinOp ($2, $1, $3, mk_position 1 3) }
;

add_op:
| PLUS { Plus }
| MINUS { Minus }

add_term: (* term@4 := term@4 aop term@5 *)
| mul_term { $1 }
| add_term add_op mult_term { BinOp ($2, $1, $3, mk_position 1 3) }
;

comp_op:
| EQ { Eq }
| NE { Ne }
| LE { Le }
| GE { Ge }
| LT { Lt }
| GT { Gt }
;

comp_term: (* term@3 := term@3 cop term@4 *)
| add_term { $1 }
| comp_term comp_op add_term { BinOp ($2, $1, $3, mk_position 1 3) }
;

and_term: (* term@2 := term@3 && term@2 *)
| comp_term { $1 }
| comp_term AND and_term { BinOp ($2, $1, $3, mk_position 1 3) }
;

or_term: (* term@1 := term@2 || term@1 *)
| and_term { $1 }
| and_term OR or_term { BinOp (Or, $1, $3, mk_position 1 3) }
;

ite_term:
| or_term { $1 }
| IF term THEN term ELSE term { Ite ($2, $4, $6, mk_position 1 6) }
;

lambda_term:
| ite_term { $1 }
| FUN IDENT ARROW term { Lambda ($2, $4, mk_position 1 4) }
;

let_in_term:
| lambda_term { $1 }
| LET REC ident_list EQ term IN term {
  let fn_rec =
    List.fold_right
      (fun x t -> Lambda (x, t, mk_position 1 7))
      $3 $5
  in
  let fn = App (FunConst (Fix, mk_position 1 7), fn_rec, mk_position 1 7) in
  App (Lambda (List.hd $3, $7, mk_position 1 7), fn, mk_position 1 7)
}
| LET ident_list EQ term IN term {
  let fn =
    List.fold_right
      (fun x t -> Lambda (x, t, mk_position 1 6))
      (List.tl $2) $4
  in
  App (Lambda (List.hd $2, $6, mk_position 1 6), fn, mk_position 1 6)
}
;

term:
| let_in_term { $1 }
| error { fail (mk_position 1 1) "Syntax error" }
;
