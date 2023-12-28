%{
(* header *)
open Syntax
open EffectAutomataSyntax
open Lexing


let convert_var_name_apron_not_support s = 
  if String.contains s '\'' then
    let lst = String.split_on_char '\'' s in
    let lst' = List.fold_right (fun s rlst -> 
      if String.length s = 0 then List.append rlst ["_pm"]
      else List.append rlst [s; "_pm_"]) lst [] in
    let restr = 
      let tempstr = String.concat "" (List.rev lst') in
      String.sub tempstr 4 (String.length tempstr - 4)
    in
    restr
  else s

%}

(* declarations *)
%token <string> IDENT
%token <int> INTCONST
%token <bool> BOOLCONST
%token LPAREN RPAREN
%token SEMI COMMA LSQBR RSQBR  
%token IF THEN ELSE FUN
%token PLUS MINUS TIMES DIV MOD 
%token EQ NE LE LT GE GT
%token AND OR
%token ARROW
%token BEGIN END
%token EMPTYLIST
%token QSET                                  (* QSet *)
%token DELTA                                 (* delta *)
%token ASSERT                                (* assert *)
%token ASSERTFINAL                           (* assertFinal *)
%token INICFG                                (* IniCfg *)
%token EOF

%nonassoc THEN
%nonassoc ELSE
%left PLUS 
%left MINUS
%left TIMES DIV MOD

%start <EffectAutomataSyntax.aut_spec> top
%%

/* production rules */
top:
  | fields=aut_fields EOF { effect_aut_spec fields }
  
aut_fields: 
  | qs=qset SEMI df=delta_fn SEMI c0=config0 SEMI assts=assts { (qs, df, c0, assts) }

qset:
  | QSET EQ LSQBR qs=separated_nonempty_list(SEMI, q=INTCONST { q }) RSQBR 
      { List.map (fun q -> Q q) qs }

delta_fn:
  | DELTA EQ FUN x=var LPAREN q=var COMMA acc=vars RPAREN ARROW e=exp 
      { SemActions.delta_fn x (q, acc) e }

config0:
  | INICFG EQ LPAREN e1=exp COMMA e2=exp RPAREN { SemActions.initial_cfg (e1, e2) }

assts:
  | ea=asst { [ea] }
  | ea=asst eas=assts { ea::eas }

asst:
  | ASSERT EQ af=asst_fun SEMI 
    { let (q, acc, e) = af in SemActions.prop_assert (q, acc) e }
  | ASSERTFINAL EQ af=asst_fun SEMI 
    { let (q, acc, e) = af in SemActions.prop_assert_final (q, acc) e } 

asst_fun: 
  | FUN LPAREN q=var COMMA acc=vars RPAREN ARROW e=bool_exp { (q, acc, e) }

exp:
  | c=const_exp { c }
  | x=var { x }
  | EMPTYLIST { Const (IntList [], "") }
  | e=if_exp { e } 
  | e=tuple_exp { e }
  | e=binary_exp { e }
  | e=unary_exp { e }
  | LPAREN e=exp RPAREN { e }
  | BEGIN e=exp END { e } 

vars:
  | x=var { [x] }
  | LPAREN x=var COMMA xs=separated_nonempty_list(COMMA, x=var { x }) RPAREN { x::xs }

var:
  | x=IDENT
      { let res_str = convert_var_name_apron_not_support x in 
        Var (res_str, "") }

const_exp:
  | i=INTCONST { Const (Integer i, "") }

tuple_exp:
  | LPAREN e=exp COMMA es=separated_nonempty_list(COMMA, e=exp { e }) RPAREN { TupleLst (e::es, "") }

%inline if_exp:
  | IF be=bool_exp THEN e1=exp ELSE e2=exp 
      { Ite (be, e1, e2, "") }
  | IF be=bool_exp THEN e1=exp 
      { let else_term = Const (UnitLit, "") in
        Ite (be, e1, else_term, "") }

bool_exp: 
  | e1=bool_exp bop=bool_op e2=bool_exp1 { BinOp (bop, e1, e2, "") } 
  | e=bool_exp1 { e }

bool_exp1:
  | b=BOOLCONST { Const (Boolean b, "") }
  | e=comp_exp { e }
  | LPAREN e=bool_exp RPAREN { e }

bool_op:
  | AND { And }
  | OR { Or }

comp_exp:
  | e1=exp op=comp_op e2=exp { BinOp (op, e1, e2, "") } 

%inline comp_op:
  | EQ { Eq }
  | NE { Ne }
  | LE { Le }
  | LT { Lt }
  | GE { Ge }
  | GT { Gt }

unary_exp:
  | op=uop e=exp { UnOp (op, e, "") }

%inline uop:
  | MINUS { UMinus }

binary_exp:
  | e1=exp op=bop e2=exp { BinOp (op, e1, e2, "") }

%inline bop:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Mult }
  | MOD { Mod }
  | DIV { Div }


