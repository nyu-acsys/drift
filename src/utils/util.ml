
let uncurry f (x, y) = f x y

module StringSet = Set.Make(struct
    type t = string
    let compare = compare
  end)

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)
    
(** Utility functions on option types *)
module Opt = struct
  let to_list = function
    | Some x -> [x]
    | None -> []

  let get = function
    | Some x -> x
    | None -> failwith "Opt.get applied to None"

  let get_or_else default = function
    | Some x -> x
    | None -> default

  let lazy_get_or_else f y = function
    | Some x -> x
    | None -> f y

  let or_else y = function
    | Some x -> Some x
    | None -> Some y

  let lazy_or_else f y = function
    | Some x -> Some x
    | None -> f y

  let fold f init = function
    | Some x -> f init x
    | None -> init

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let flat_map f = function
    | Some x -> f x
    | None -> None

  let iter f = function
    | Some x -> f x
    | None -> ()

  let some x = function
    | None -> Some x
    | o -> o
  
  let exist = function
    | None -> false
    | Some _ -> true
end

let rec zip_list xs ys =
  match xs with
      [] -> (match ys with
              [] -> []
            | y::ys' -> failwith "oops, the lists seems to have different lengths")
    | x::xs' -> (match ys with
            [] -> failwith "oops, the lists seems to have different lengths"
          | y::ys' -> (x,y) :: zip_list xs' ys')

let rec extract k list =
    if k <= 0 then [ [] ]
    else match list with
         | [] -> []
         | h :: tl ->
            let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
            let without_h = extract k tl in
            with_h @ without_h
(* l1 <> "l" && contain_var_R l1 rl2 && contain_var_R l2 rl1 then
        let l', e' = fresh_length (), fresh_item () in
        let lst1' = alpha_rename_Lst lst1 l1 l' in
        let lst2' = alpha_rename_Lst lst2 l2 l' in
        alpha_rename_Lst lst1' e1 e', alpha_rename_Lst lst2' e2 e'
        else if *)
let arrange list = 
    List.fold_right (fun y acc -> 
      List.fold_right (fun x acc -> [x; y] :: acc) list acc) list []

let fresh_func var =
  let ref_index = ref 0 in
  fun () ->
    let idx = !ref_index in
    incr ref_index;
    var ^ (string_of_int idx)



let measured_time = ref 0.
let measured_calls = ref 0

(** measure accumulated execution time and number of calls to a particular function *)
let measure fn arg =
  let start_time = 
    let ptime = Unix.times () in
    ptime.tms_utime
  in
  try
    let res = fn arg in
    let end_time = 
      let ptime = Unix.times () in
      ptime.Unix.tms_utime
    in
    measured_time := !measured_time +. (end_time -. start_time);
    incr measured_calls;
    res
  with e ->
    let end_time = 
      let ptime = Unix.times () in
      ptime.Unix.tms_utime
    in
    measured_time := !measured_time +. (end_time -. start_time);
    incr measured_calls;
    raise e

let measures = Hashtbl.create 10

let measure_call (id: string) fn arg =
  let get_time () = 
    let ptime = Unix.times () in
    ptime.tms_utime
  in
  let (calls, time) =
    if Hashtbl.mem measures id
    then Hashtbl.find measures id
    else (0, 0.)
  in
  let start_time = get_time () in
  try
    let res = fn arg in
    let end_time = get_time () in
    Hashtbl.replace measures id (calls + 1, time +. end_time -. start_time);
    res
  with e ->
    let end_time = get_time () in
    Hashtbl.replace measures id (calls + 1, time +. end_time -. start_time);
    raise e

let print_measures () =
  if Hashtbl.length measures > 0 then print_endline "Profiling:";
  Hashtbl.iter
    (fun id (calls, time) ->
      print_endline ("  " ^ id ^ ": " ^ (string_of_int calls) ^ " call(s), " ^ (string_of_float time) ^ " s")
    )
    measures
