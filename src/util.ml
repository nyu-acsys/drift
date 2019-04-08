(** {0} Utility functions *)

(** source code position, line:column *)
type pos = { pos_line: int; pos_col: int }

(** a dummy source code position *)
let dummy_pos = { pos_line = 0; pos_col = 0 }

(** Create an error message for a given source code position [pos] and string [msg]. 
  * Then throw it as a [Failure] exception. *)
let fail pos msg = failwith (Printf.sprintf "Error:%d:%d: %s" pos.pos_line pos.pos_col msg)

    
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
end
