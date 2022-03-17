(* Version string *)
let version = "0.1 beta"

let debug = ref false

let out_put_level = ref 0 (* default to print map after each step *)

let bt = ref false

let parse_file = ref false

let file = ref ""

let delay_wid = ref 0

let narrow = ref false

let domain = ref "Oct" (*Default: Octagon domain*)

let use_threshold = ref false

let sensitive = ref false

let no_simplify = ref false

let color = ref false

let cmd_options_spec =
  [("-file", Arg.String (fun s -> parse_file := true; file := s), ": Input file specification");
   ("-domain", Arg.String (fun s -> domain := s), ": Abstract domain specification (Oct, Polka_st, Polka_ls, OctPolka)");
   ("-sen", Arg.String (fun s -> if s = "true" then sensitive:=true else sensitive:=false), ": Use 1-context sensitive analysis");
   ("-thold", Arg.String (fun s -> if s = "true" then use_threshold:=true else use_threshold:=false), ": Use threshold widening");
   ("-delay-wid", Arg.Int (fun s -> delay_wid := s), ": Set number of delay widening steps (depricated)");
   ("-nar", Arg.String (fun s -> if s = "true" then narrow:=true else narrow:=false), ": Use narrowing procedure");
   ("-color", Arg.Set color, ": Print output in color");
   ("-out", Arg.Int (fun s -> out_put_level := s),
    ": Output result level
      \t 0: Output map after each step
      \t 1: Output map only for the last step
      \t 2: Output the result only");
   ("-debug", Arg.Set debug, ": Debug mode");
   ("-bt", Arg.Set bt, ": Allow trace back");
   ("-no-simplify", Arg.Set no_simplify, ": Disable simplification of program expression before analysis");
  ]

(* Parse auxiliary 'command line options' that are set during parsing of the input file *)
let parse_options options =
  (if (!debug) then Printf.printf "Setting options: %s\n" options);
  let options = Sys.argv.(0) :: Str.split_delim (Str.regexp "[ \t\n]+") options |> Array.of_list in
  let current = ref 0 in
  try Arg.parse_argv ~current:current options cmd_options_spec (fun _ -> ()) ""
  with Arg.Bad full_msg ->
    let regexp = Sys.argv.(0) ^ ": \\([^\\.]*\\)" in
    let matched = Str.string_match (Str.regexp regexp) full_msg 0 in
    let msg =
      if matched then Str.matched_group 1 full_msg 
      else "invalid option"
    in
    raise (Invalid_argument msg)


let parse = Arg.parse cmd_options_spec (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) ("Usage: " ^ Sys.argv.(0));
