


module type SenManagerType =
  sig
    type node
    type env
    type node_s
    type 'a table
    type call_site
    val init_T: 'a table
    val alpha_rename_T
    val join_T
    val meet_T
    val leq_T
    val eq_T
    val forget_T
    val wid_T
    val equal_T
    val replace_T
    val stren_T
    val proj_T
  end

module type SensitiveSemanticsType =
  sig
    type t
    type node_t
    type node_s_t
    type table_t
    val init
    val alpha_rename
    val join
    val meet
    val leq
    val eq
    val forget
    val wid
    val equal
    val replace
    val stren
    val proj
  end

module MakeTableSemantics (Man: SenManagerType): SensitiveSemanticsType = 
  struct

  end


module NonSensitive: SenManagerType =
  struct
    type node = EN of env * loc (*N = E x loc*)
    and env = node VarMap.t (*E = Var -> N*)
    type node_s = SN of bool * loc
    type 'a table = var * 'a
    type call_site = var
  end

module OneSensitive: SenManagerType =
  struct
    type enode = env * loc (*N = E x loc*)
    and vnode = env * var * call_site (*Nx = E x var x stack*)
    and node = EN of enode | VN of vnode
    and env = node VarMap.t (*E = Var -> N*)
    and call_site = var * loc   (*stack = var * loc*)
    type node_s = SEN of (var * loc) | SVN of (var * var * loc) (* call site * label *)
    type 'a table = 'a TableMap.t
  end


let parse_sensitive = function
  | 0 -> 
  | 1 ->
  | _ -> raise (Invalid_argument "Incorrect sensitive specification")

module SenSemantics = (val (!sensitive |> parse_sensitive))