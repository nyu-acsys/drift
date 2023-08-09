open AbstractDomain
open List
open DriftSyntax
open Util
open Format

type loc_token_t = 
  | None_Loc_Token of loc
  | If_True of loc
  | If_False of loc
  | If_End of loc
  | Pat_Case of loc * loc
  | Pat_End of loc

type trace_t = loc_token_t list

type loc_tree = 
  | Empty
  | Leaf of loc_token_t
  | Node of loc_token_t * (loc_tree list)

type loc_tree_t = loc * loc_tree

let create_singleton_trace loc = [None_Loc_Token loc]

let get_loc_token_loc loc_token = match loc_token with
  | None_Loc_Token loc | If_True loc | If_False loc | If_End loc | Pat_Case (loc,_) | Pat_End loc -> loc

let get_trace_tree_token tree : loc_token_t = match tree with
  | Empty -> raise (Invalid_argument "Empty tree")
  | Leaf token -> token
  | Node (token, _) -> token

let comp_loc_token token1 token2 = 
  let loc1 = get_loc_token_loc token1 in
  let loc2 = get_loc_token_loc token2 in
  let loc_comp = String.compare loc1 loc2 in
  if loc_comp != 0 then loc_comp else
  (
    match token1 with
    | None_Loc_Token _ -> 
      (
        match token2 with
        | None_Loc_Token _ -> 0
        | _ -> 1
      )
    | If_True _ -> 
      (
        match token2 with
        | None_Loc_Token _ -> -1
        | If_True _ -> 0
        | _ -> 1
      )
    | If_False _ -> 
      (
        match token2 with
        | None_Loc_Token _ | If_True _ -> -1
        | If_False _ -> 0
        | _ -> 1
      )
    | Pat_Case (_, loc21) ->
      (
        match token2 with
        | If_End _ | Pat_End _ -> 1
        | Pat_Case (_, loc22) -> String.compare loc21 loc22
        | _ -> -1
      )
    | If_End _ -> 
      (
        match token2 with
        | Pat_End _ -> 1
        | If_End _ -> 0
        | _ -> -1
      )
    | Pat_End _ ->
      (
        match token2 with
        | Pat_End _ -> 0
        | _ -> -1
      )
  )

let is_if_branch_token token = match token with
  | If_True _ | If_False _ -> true
  | _ -> false

let is_pat_branch_token token = match token with
  | Pat_Case _ -> true
  | _ -> false

let rec comp_trace trace1 trace2 = match trace1, trace2 with 
  | [], [] -> 0
  | [], _ -> 1
  | _, [] -> -1
  | head1::tail1, head2::tail2 -> let head_result = comp_loc_token head1 head2 in
      if head_result = 0 then comp_trace tail1 tail2 else head_result

let sort_traces trace_list = List.sort(fun trace1 trace2 -> comp_trace trace1 trace2) trace_list

let sort_trees tree_list = List.sort (fun t1 t2 -> comp_loc_token (get_trace_tree_token t1) (get_trace_tree_token t2)) tree_list

let rec get_trace_data trace = match trace with
  | [] -> ""
  | head :: [] -> get_loc_token_loc head
  | head :: tail -> get_loc_token_loc head ^","^ get_trace_data tail

let print_loc_token ppf loc_token = match loc_token with
  | None_Loc_Token loc -> Format.fprintf ppf "@[None:<2>%s]" loc
  | If_True loc -> Format.fprintf ppf "@[If True:<2>%s]" loc
  | If_False loc -> Format.fprintf ppf "@[If False:<2>%s]" loc
  | If_End loc -> Format.fprintf ppf "@[If End:<2>%s]" loc
  | Pat_Case (loc1, loc2) -> Format.fprintf ppf "@[Pattern Matching case:<2>%s <2>%s]" loc1 loc2
  | Pat_End loc -> Format.fprintf ppf "@[Pattern Matching End:<2>%s]" loc

let rec print_trace ppf trace = match trace with
  | [] -> ()
  | head :: tail -> (print_loc_token ppf head; print_trace ppf tail)

let find_head_in_tree_list tree_list token = List.find (fun tree -> get_trace_tree_token tree = token) tree_list

let rec trace_to_tree trace = match trace with
  | [] -> Empty
  | head :: [] -> Leaf head
  | head :: tail -> Node (head, [trace_to_tree tail])

(* assumes that tree and trace have common head *)
let rec add_trace_to_tree tree trace = match tree, trace with 
  | Empty, _ -> raise (Invalid_argument "add_trace_to_tree: Empty tree")
  | _, [] -> raise (Invalid_argument "add_trace_to_tree: Empty trace")
  | Leaf token, head :: tail -> if token = head then tree else raise (Invalid_argument "add_trace_to_tree: heads are not common")
  | Node (token, children), head :: tail -> if token = head 
    then Node (token, add_tree_to_node_list children tail)
    else raise (Invalid_argument "add_trace_to_tree: heads are not common")

(* assumes that no trace is a subtrace of another *)
and add_tree_to_node_list nodes trace = match nodes with
  | [] -> [trace_to_tree trace]
  | head_trace :: tail -> 
    let comp_res = get_trace_tree_token head_trace |> comp_loc_token (List.hd trace) in
    if comp_res = 0 
      then add_trace_to_tree head_trace trace :: tail
      else if comp_res = -1 then head_trace :: add_tree_to_node_list tail trace
      else trace_to_tree trace :: nodes
          
(* assumes that trace_list is sorted *)
let rec merge_trace_list trace_list res_list = match trace_list with
  | [] -> res_list
  | head :: tail -> add_tree_to_node_list res_list head |> merge_trace_list tail

let collect_traces (data: loc_token_t list list) = 
  let sorted_traces = List.sort(fun trace1 trace2 -> comp_trace trace1 trace2) data in
  merge_trace_list sorted_traces []

let rec uncollect_traces tree = match tree with
  | Empty -> []
  | Leaf token -> [[token]]
  | Node (token, children) -> List.concat_map (fun child -> List.map (fun trace -> token :: trace) (uncollect_traces child)) children

let rec longest_common_subtrace trace1 trace2 = match trace1, trace2 with
  | [], _ -> []
  | _, [] -> []
  | head1 :: tail1, head2 :: tail2 -> if head1 = head2 then head1 :: (longest_common_subtrace tail1 tail2) else []

(* if trace is larger than tree, empty list is returned *)
let rec find_trace_tree_super_traces trace tree = match tree, trace with
  | Empty, _ -> []
  | _, [] -> raise (Invalid_argument "find_trace_tree_super_traces: empty trace")
  | Leaf token, [trace_token] -> if trace_token = token then [trace] else []
  | Leaf token, _ -> []
  | Node (token, children), head :: [] -> List.concat_map uncollect_traces children
  | Node (token, children), head :: tail -> find_head_in_tree_list children head |> find_trace_tree_super_traces tail

(* assumes that token is there in trace *)
let rec find_subtrace_with_token trace token = match trace with
  | [] -> raise (Invalid_argument "find_subtrace_with_token: Empty trace")
  | head :: tail -> if head = token then trace else find_subtrace_with_token tail token

(* assumes that root is there in tree *)
let rec find_subtrees_with_token trees root = match trees with
  | [] -> []
  | tree :: tail -> find_subtree_with_token tree root @ find_subtrees_with_token tail root

and find_subtree_with_token tree token = match tree with
  | Empty -> raise (Invalid_argument "find_subtree_with_root: Empty tree")
  | Leaf tok -> [tree]
  | Node (tok, children) -> if tok = token then [tree] else find_subtrees_with_token children token

(* assumes that root is equal *)
let rec join_trace_trees trace_tree1 trace_tree2 = match trace_tree1, trace_tree2 with
  | Empty, _ -> raise (Invalid_argument "join_trace_trees: Empty tree1")
  | _, Empty -> raise (Invalid_argument "join_trace_trees: Empty tree2")
  | Leaf token1, _ -> trace_tree1
  | _, Leaf token2 -> trace_tree2
  | Node (token1, children1), Node (token2, children2) -> let sub_children = join_trace_subtree_lists children1 children2 in
      if List.length sub_children = 0 then Leaf (token1) else Node (token1, sub_children)

(* this doesn't return any non-matched trees. Useful for when all non-matched trees have to be discarded. *)
and join_trace_subtree_lists children1 children2 = 
  let concat_children_lists tree_list = 
    List.concat_map (fun tree_node -> match tree_node with
      | Empty -> []
      | Leaf _ -> []
      | Node (_, children) -> children
    ) tree_list 
  in
  (* if all trees are not matched. Then the following cases are possible:
      1. children1 is divided by if branch at some loc l or a pattern matching branch at some loc l, and children2 has a single token non_loc_token(l). 
      2. children1 and children2 have different location non_loc_tokens. In this case, everything should be merged.
      Note that traces in both lists can't have different locations. We ensure that even if partitions are not created at if-else, a non token is added. *)
  let matches, no_matches = join_trace_tree_lists children1 children2 [] [] in
  match matches, no_matches with
  | [], [] -> raise (Invalid_argument "join_trace_trees: Empty children")
  | [], tree :: tail -> 
      let tree_loc = get_trace_tree_token tree |> get_loc_token_loc in
      if List.for_all (fun tree -> get_trace_tree_token tree |> get_loc_token_loc = tree_loc) tail
        (* here we will merge the partitioned if branch of one of the 2 children. This is necessary to preserve any partitions down the tree that might be there
          in both children. *)
        then 
          let if_case = (List.hd children1 |> get_trace_tree_token |> is_if_branch_token |> not) || (List.hd children2 |> get_trace_tree_token |> is_if_branch_token |> not) in
          let end_token = if if_case then If_End tree_loc else Pat_End tree_loc in
          (* assumes homogenity. This is fine because both set of children have a single root *)
          let subtrees1, subtrees2 = 
          if (List.hd children1 |> get_trace_tree_token |> is_if_branch_token |> not) || (List.hd children1 |> get_trace_tree_token |> is_pat_branch_token |> not)
            then find_subtrees_with_token children1 end_token, children2
            else find_subtrees_with_token children2 end_token, children1
          in
            let sub_children = join_trace_subtree_lists (concat_children_lists subtrees1) (concat_children_lists subtrees2) in
            if List.length sub_children = 0 then [Leaf (None_Loc_Token (tree_loc))] else [Node (None_Loc_Token (tree_loc), sub_children)]
        else []
  | tree :: tail, [] -> matches
  | tree :: tail, _ -> []

(* this doesn't returns non-matched trees. Useful for when non-matched trees don't have to be discarded. *)
and join_trace_tree_lists trace_tree_list1 trace_tree_list2 matches no_matches = 
  let tree_token1 = List.hd trace_tree_list1 |> get_trace_tree_token in
  let tree_token2 = List.hd trace_tree_list2 |> get_trace_tree_token in
  match trace_tree_list1, trace_tree_list2 with
  | [], _ -> matches, no_matches @ trace_tree_list2
  | _, [] -> matches, no_matches @ trace_tree_list1
  | trace_tree1 :: tail1, trace_tree2 :: tail2 -> 
    let comp_res = comp_loc_token tree_token1 tree_token2 in
    if comp_res = 0
      then join_trace_tree_lists tail1 tail2 (matches @ [join_trace_trees trace_tree1 trace_tree2]) no_matches
      else if comp_res < 0 then no_matches @ [trace_tree1] |> join_trace_tree_lists tail1 trace_tree_list2 matches
      else no_matches @ [trace_tree2] |> join_trace_tree_lists trace_tree_list1 tail2 matches

let join_traces traces1 traces2 = 
  let trees1 = sort_traces traces1 |> collect_traces in
  let trees2 = sort_traces traces2 |> collect_traces in
  let matched, non_matched = join_trace_tree_lists trees1 trees2 [] [] in
  matched @ non_matched

(* assumes that root is equal *)
let rec meet_trace_trees trace_tree1 trace_tree2 = match trace_tree1, trace_tree2 with
  | Empty, _ -> raise (Invalid_argument "meet_trace_trees: Empty tree1")
  | _, Empty -> raise (Invalid_argument "meet_trace_trees: Empty tree2")
  | Leaf token1, _ -> trace_tree2
  | _, Leaf token2 -> trace_tree1
  | Node (token1, children1), Node (token2, children2) -> Node (token1, meet_trace_subtree_lists children1 children2)

and meet_trace_subtree_lists children1 children2 = 
  let matches, no_matches = meet_trace_tree_lists children1 children2 [] [] in
  match matches, no_matches with
  | [], [] -> raise (Invalid_argument "meet_trace_subtree_lists: Empty children")
  | [], tree :: tail -> 
      let tree_loc = get_trace_tree_token tree |> get_loc_token_loc in
      if List.for_all (fun tree -> get_trace_tree_token tree |> get_loc_token_loc = tree_loc) tail
        then 
          let non_loc_token = None_Loc_Token tree_loc in
          let if_case = (List.hd children1 |> get_trace_tree_token |> is_if_branch_token |> not) || (List.hd children2 |> get_trace_tree_token |> is_if_branch_token |> not) in
          let end_token = if if_case then If_End tree_loc else Pat_End tree_loc in
          (* forcing subtrees1 to be the one with non_loc_token *)
          let subtrees1, subtrees2 = 
          if (List.hd children1 |> get_trace_tree_token |> is_if_branch_token |> not) || (List.hd children1 |> get_trace_tree_token |> is_pat_branch_token |> not)
            then find_subtrees_with_token children1 non_loc_token, children2
            else find_subtrees_with_token children2 non_loc_token, children1
          in
          List.concat_map (fun branch -> meet_non_loc_at_case_end branch subtrees1 end_token) subtrees2
        else no_matches
  | tree :: tail, [] -> matches
  | tree :: tail, _ -> sort_trees (matches @ no_matches)

and meet_trace_tree_lists trace_tree_list1 trace_tree_list2 matches no_matches = 
  let tree_token1 = List.hd trace_tree_list1 |> get_trace_tree_token in
  let tree_token2 = List.hd trace_tree_list2 |> get_trace_tree_token in
  match trace_tree_list1, trace_tree_list2 with
  | [], _ -> matches, no_matches @ trace_tree_list2
  | _, [] -> matches, no_matches @ trace_tree_list1
  | trace_tree1 :: tail1, trace_tree2 :: tail2 -> 
    let comp_res = comp_loc_token tree_token1 tree_token2 in
    if comp_res = 0
      then meet_trace_tree_lists tail1 tail2 (matches @ [meet_trace_trees trace_tree1 trace_tree2]) no_matches
      else if comp_res < 0 then no_matches @ [trace_tree1] |> meet_trace_tree_lists tail1 trace_tree_list2 matches
      else no_matches @ [trace_tree2] |> meet_trace_tree_lists trace_tree_list1 tail2 matches
    
and meet_non_loc_at_case_end if_tree non_loc_children end_token = match if_tree with
  | Empty -> raise (Invalid_argument "meet_non_loc_at_if_end: Empty if tree")
  | Leaf tok -> if tok = end_token then [Node (tok, non_loc_children)] else raise (Invalid_argument "meet_non_loc_at_if_end: if end missing")
  | Node (tok, children) -> 
    if tok = end_token 
      then [Node (tok, meet_trace_subtree_lists children non_loc_children)]
      else List.concat_map (fun if_child -> [Node (tok, meet_non_loc_at_case_end if_child non_loc_children end_token)]) children

let meet_traces traces1 traces2 = 
  let trees1 = sort_traces traces1 |> collect_traces in
  let trees2 = sort_traces traces2 |> collect_traces in
  let matched, _ = meet_trace_tree_lists trees1 trees2 [] [] in
  matched

(* assumes tree, trace head is always equal (the case where the tree's if/pattern partition is merged to none_loc is handled separately). 
   if common trace doesn't end at leaf, a Not_Found error is raised *)
let rec find_trace_tree_longest_common_trace trace tree = match tree, trace with
  | _, [] -> []
  | Empty, _ -> raise (Invalid_argument "find_trace_tree_longest_common_trace: given empty tree")
  | Leaf token, head :: tail -> [token]
  | Node (token, children), head :: tail -> 
      if token = List.hd trace
        then token :: (List.hd tail |> find_head_in_tree_list children |> find_trace_tree_longest_common_trace tail)
        else 
          let if_else_token = List.hd trace in 
          let if_end_token =  If_End (get_loc_token_loc if_else_token) in
          let subtrace = find_subtrace_with_token trace if_end_token in
          token :: (List.tl subtrace |> List.hd |> find_head_in_tree_list children |> find_trace_tree_longest_common_trace (List.tl subtrace))