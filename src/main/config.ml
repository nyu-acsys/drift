(* Version string *)
let version = "0.1 beta"

let debug = ref false

let bt = ref false

let domain = ref "Box"

let cmd_options_spec =
  [("-debug", Arg.Set debug, "Debug mode (output results)");
  ("-bt", Arg.Set bt, "BackTrace mode");
  ("-domain", Arg.String (fun s -> domain := s), ": Domain specification")]

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