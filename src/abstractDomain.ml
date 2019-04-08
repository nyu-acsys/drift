
open Apron

(* Define Abstract Domain Functor*)
module type ManagerType =
  sig
    type t
    val man: t Manager.t
  end

module BoxManager: ManagerType =
  struct
    type t = Box.t
    let man = Box.manager_alloc ()
  end

module type AbstractDomainType =
  sig
    type t
    val leq: t -> t -> bool
  end

module MakeAbstractDomain (Man: ManagerType): AbstractDomainType =
  struct
    type t = Man.t Abstract1.t
    (*val initialize : 'a list -> unit*)
    let leq v1 v2 = Abstract1.is_leq Man.man v1 v2
    (* let rec prop v1 v2 =  *)
    (* val bot : unit -> t *)
    (* val join :  t -> t -> t *)
  end

(* Define type of Table and Execution Map*)
type loc = int
type var = string

module VarMap = Map.Make(struct
    type t = var
    let compare = compare
end)

type value = MakeAbstractDomain(BoxManager).t

type node = EN of env * loc

and env = node VarMap.t

module SigmaOrder : Set.OrderedType = struct
  type dvar = var
  type t = dvar * value * value
  let compare = compare
end

type table = Set.Make(SigmaOrder).t

module NodeMap = Map.Make(struct
    type t = node
    let compare = compare
end)

module PiOrder : Set.OrderedType = struct
  type t = value NodeMap.t
  let compare = compare
end

type exec_map = Set.Make(PiOrder).t
(*TODO: Add module for Execution map, also join, initialize, etc.*)
