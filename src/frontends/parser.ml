(** {0} Interface for MiniML Parser *)

(** Parse MiniML term from file given by path [file]. *)
let parse_from_file file =
  try
    let chan = open_in file in
    let lexbuf = Lexing.from_channel chan in
    let t = Grammar.main Lexer.token lexbuf in
    let _ = close_in chan in
    t
  with Sys_error _ ->
    failwith ("Could not find file " ^ file)

(** Parse MiniML term from given string [s]. *)
let parse_from_string s =
  let lexbuf = Lexing.from_string s in
  Grammar.main Lexer.token lexbuf
