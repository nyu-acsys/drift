(** Grammar rules for lexer of Effect Automata Specs *)
{
open Util
open EffectAutomataSyntax
open EffectAutomataGrammar
open Lexing

let keyword_table = Hashtbl.create 32
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    ([("begin", BEGIN);
      ("else", ELSE);
      ("end", END);
      ("false", BOOLCONST false);
      ("assert", ASSERT);
      ("fun", FUN);
      ("if", IF);
      ("mod", MOD);
      ("then", THEN);
      ("true", BOOLCONST true);
      ("QSet", QSET);
      ("delta", DELTA);
      ("IniCfg", INICFG)])

let lexical_error lexbuf msg =
  let pos = lexeme_start_p lexbuf in 
  let pos_line = pos.pos_lnum in
  let pos_col = pos.pos_cnum - pos.pos_bol in
  let pos':pos = { pl = pos_line; pc = pos_col } in
  fail pos' "Syntax error"
}

let white_space = [' ']*
let basic_type = "int" | "bool" | "unit"
let types = "int" | "bool" | "unit"
let digitchar = ['0'-'9']
let idchar = ['A'-'Z''a'-'z''_']
let primechar = [''']
let minus = ['-']
let ident = idchar(idchar | digitchar)*('.'idchar)?(idchar | digitchar | primechar)* 
let digits = digitchar+
let bool_str = "true" | "false" | "either"
let bin_op_str =  ['>''<''='] | "<=" | ">=" | "<>" 
let arth_op_str = ['+''-''*''/'] | "mod" | "&&" | "||" 
let empty_list = "[" white_space "]"

rule token = parse
  [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "(*" { comments 0 lexbuf }
| ","  { COMMA }
| ";"  { SEMI }
| "->" { ARROW }
| "<=" { LE }
| ">=" { GE }
| "&&" { AND }
| "||" { OR }
| "=" { EQ }
| "<>" { NE }
| "<" { LT }
| ">" { GT }
| "::" { CONS }
| "[" { LSQBR }
| "]" { RSQBR }
| '+' { PLUS }
| '-' { MINUS }
| '/' { DIV }
| '*' { TIMES }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ident as kw
    { try
      Hashtbl.find keyword_table kw
    with Not_found ->
      IDENT (kw)
    }
| empty_list { EMPTYLIST }
| digits as num { INTCONST (int_of_string num) }
| eof { EOF }
| _ { lexical_error lexbuf None }

and comments level = parse
| "*)" { if level = 0 then token lexbuf
         else comments (level - 1) lexbuf
       }
| "(*" { comments (level + 1) lexbuf }
| '\n' { Lexing.new_line lexbuf; comments (level) lexbuf }
| _ { comments level lexbuf }
| eof { token lexbuf }
