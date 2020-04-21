(* Version string *)
let version = "0.1 beta"

let debug = ref false

let integrat_test = ref false

let bt = ref false

let parse_file = ref false

let file = ref ""

let delay_wid = ref 0

let narrow = ref false

let domain = ref "Oct" (*Default: Octagon domain*)

let usage = "usage: ./tests.native [-file input_file_name] [-domain domain_name] [-nar true/false] [-delay-wid NUM] [-debug] [-bt] [-int]"

let cmd_options_spec =
  [("-debug", Arg.Set debug, ": Debug mode");
  ("-bt", Arg.Set bt, ": Allow trace back");
  ("-domain", Arg.String (fun s -> domain := s), ": Domain specification");
  ("-delay-wid", Arg.Int (fun s -> delay_wid := s), ": Set delay widening steps");
  ("-int", Arg.Set integrat_test, ": Display the final result only");
  ("-nar", Arg.String (fun s -> if s = "true" then narrow:=true else narrow:=false), ": Use narrowing procedure");
  ("-file", Arg.String (fun s -> parse_file := true; file := s), ": Input file specification")]

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


let parse = Arg.parse cmd_options_spec (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;