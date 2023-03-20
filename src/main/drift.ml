open AbstractDomain
open SemanticDomain
open AbstractTransformer
open DriftSyntax
open Config
open Util
open DriftParser
open Printer

let x = "x"
let y = "y"
let f = "f"
let g = "g"
let r = "r"
let dec = "dec"
let id = "id"
let loop = "loop"

let tests = []
  
let _ =
  try
    Config.parse;
    Printexc.record_backtrace !Config.bt;
    let t = if !Config.parse_file then 
      begin
        pre_vars := VarDefMap.empty;
        thresholdsSet := ThresholdsSetType.empty;
        [parse_from_file !Config.file]
      end
    else tests in
    List.iter (fun e ->
      let el =
        e
        |> (if not !Config.no_simplify then
              DriftSyntax.simplify
            else
              Fun.id)
        |> label
      in
      if !out_put_level < 2 then
        (print_endline "Executing:";
         print_exp stdout el);
      print_endline "\n";
      print_endline ("Domain specification: " ^ AbstractValue.name);
      print_endline "\n";
      (* exit 0; *)
      if !Config.effect_tr then
        let tre = Translation.tr el (Translation.simpl_acc_t) (Translation.simpl_cfg0) in
        (print_endline "Translated program:";
         print_exp stdout tre); 
      else
        ignore (semantics el);
      if !out_put_level < 2 then
        print_exp stdout el;
      print_newline ()) t;
    if !Config.debug then print_measures ()
  with
  | Sys_error s | Failure s -> 
      let bs = if !Config.debug then Printexc.get_backtrace () else "" in
      output_string stderr ("Fatal Error: " ^ s ^ "\n" ^ bs); exit 1
  | e ->
      let bs = if !Config.debug then Printexc.get_backtrace () else "" in
      output_string stderr ("Fatal Error: " ^ Printexc.to_string e ^ "\n" ^ bs); exit 1
