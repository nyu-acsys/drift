%{
(* header *)
(** {0} Grammar rules for parser of Mini-OCaml *)

open Util
open Syntax
open Lexing


%}
/* declarations */

%token <string> IDENT
%token <int> INTCONST
%token <bool> BOOLCONST
%token LPAREN RPAREN 
%token NOT FIX
%token PLUS MINUS DIV TIMES MOD
%token EQ NE LE GE LT GT
%token ARROW
%token IF ELSE THEN FUN LET REC IN
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

basic_term: /*term@7 := int | bool | var | (term)*/
| INTCONST { Const (Integer $1, 0) }
| BOOLCONST { Const (Boolean $1, 0) }
| IDENT { Var ($1, 0) }
| LPAREN term RPAREN { $2 } /*Parentheses*/
;

app_term: /* term@6 := term@6 term@7 | term@7 */
| basic_term { $1 }
| app_term basic_term { App ($1, $2, 0) }
;

mult_op:
| TIMES { Mult }
| DIV { Div }
| MOD { Mod }
;

mult_term: /* term@5 := term@5 mop term@6 */
| app_term { $1 }
| mult_term mult_op app_term { BinOp ($2, $1, $3, 0) }
;

add_op:
| PLUS { Plus }
| MINUS { Minus }
;

add_term: /* term@4 := term@4 aop term@5 */
| mult_term { $1 }
| add_term add_op mult_term { BinOp ($2, $1, $3, 0) }
;

comp_op:
| EQ { Eq }
| NE { Ne }
| LE { Le }
| GE { Ge }
| LT { Lt }
| GT { Gt }
;

comp_term: /* term@3 := term@3 cop term@4 */
| add_term { $1 }
| comp_term comp_op add_term { BinOp ($2, $1, $3, 0) }
;

ite_term:
| comp_term { $1 }
| IF term THEN term ELSE term { Ite ($2, $4, $6, 0) }
;

lambda_term:
| ite_term { $1 }
| FUN IDENT ARROW term { mk_lambda $2 $4 }
;

let_in_term:
| ite_term { $1 }
| LET REC ident_list EQ term IN term {
  let fn = mk_lambdas (List.tl $3) $5 in
  mk_let_rec (List.hd $3) fn $7
}
| LET ident_list EQ term IN term {
  let fn = mk_lambdas (List.tl $2) $4 in
  mk_let (List.hd $2) fn $6
}
;

term:
| let_in_term { $1 }
| error { fail 1 1 "Syntax error" }
;
