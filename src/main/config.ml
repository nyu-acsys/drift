(* Version string *)
let version = "0.2 beta"

let debug = ref false

let profile = ref false

let out_put_level = ref 0 (* default to print map after each step *)

let bt = ref false

let parse_file = ref false

let file = ref ""

let delay_wid = ref 0

let narrow = ref false

let domain = ref "Oct" (*Default: Octagon domain*)

let use_threshold = ref false

let trace_len = ref 0
let if_part = ref false
let effect_on = ref false
let io_effect_on = ref false
let ev_trans = ref false
let prop_file = ref "" 

let convert_to_cps = ref false
let convert_to_tuple = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ " [-file <file name>] [-domain <domain name>] [-thold <true/false>] [-prop <file name>] [-trace-len <num>] [-if-part <true/false>] [-io-effects <true/false>] [-ev-trans <trans/direct>] [-cps-convert <true/false>] [-tuple-convert <true/false>] [-out <num:[0,2]>] [-debug] [-bt]"

let cmd_options_spec =
  [("-file", Arg.String (fun s -> parse_file := true; file := s), ": Input file specification");
  ("-domain", Arg.String (fun s -> domain := s), ": Abstract domain specification (Oct, Polka_st, Polka_ls, PolkaGrid)");
   ("-thold", Arg.String (fun s -> if s = "true" then use_threshold:=true else use_threshold:=false), ": Use threshold widening");
   ("-trace-len", Arg.Int (fun s -> trace_len := s), ": Set maximum allowed trace length. 0 -> not context sensitive");
   ("-if-part", Arg.String (fun s -> if s = "true" then if_part:=true else if_part:=false), ": Partition traces on if tokens");
   (* ("-delay-wid", Arg.Int (fun s -> delay_wid := s), ": Set number of delay widening steps (depricated)"); *)
   (* ("-nar", Arg.String (fun s -> if s = "true" then narrow:=true else narrow:=false), ": Use narrowing procedure (depricated)"); *)
   ("-prop", Arg.String (fun s -> effect_on := true; prop_file := s), ": Automata specification of safety property for effect analysis");
   ("-ev-trans", Arg.String (fun s -> if s = "trans" then ev_trans := true else ev_trans := false), ": Translate Ev expressions");
   ("-io-effects", Arg.String (fun s -> if (s = "true" && !effect_on && (not !ev_trans)) then io_effect_on := true else io_effect_on := false), ": Input-output relations for effects for greater precision");  
   ("-cps-convert", Arg.String (fun s -> if s = "true" then (convert_to_cps:=true; ev_trans := false) 
                                      else convert_to_cps:=false), ": Convert prog to CPS");
   ("-tuple-convert", Arg.String (fun s -> if s = "true" then (convert_to_tuple:=true; ev_trans := true) 
                                      else convert_to_tuple:=false), ": Convert prog to tuple-encoding of the product program");
   ("-out", Arg.Int (fun s -> out_put_level := s), 
    ": Output result level
      \t 0: Output map after each step
      \t 1: Output map only for the last step
      \t 2: Output the result only");
   ("-debug", Arg.Set debug, ": Debug mode");
   ("-profile", Arg.Set profile, ": Profiling output");
   ("-bt", Arg.Set bt, ": Allow trace back");
  ]

(* Parse auxiliary 'command line options' that are set during parsing of the input file *)
(* let parse_options options =
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
    raise (Invalid_argument msg) *)


let parse = Arg.parse cmd_options_spec (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
if String.equal !domain "PolkaGrid" && !use_threshold then raise (Arg.Bad ("Bad argument : Thresholding with PolkaGrid domain is unsound."));
