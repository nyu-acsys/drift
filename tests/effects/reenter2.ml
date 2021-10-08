let ev d _s = 
   _s + d

let nondet () : bool = ...

(* infer return value ((), int) always positive *)
let rec reenter d _s0 =
   if d > 0 then begin
     let _s1 = ev 1 _s0 in
     if nondet () then 
        let _s2 = reenter (d - 1) _s1 in 
        ev (-1) _s2 
     else 
        _s1
  end else _s0

let main (d:int) = 
  let _s0 = 0 in
  let _s2 = reenter d _s0 in
   ev (-1) _s1

(*

User writes:
   main : int -- (sum(d) > 0) --> unit
- contribution: compiling assertion language to drift
- need to add References

grammar
- max seq -> int
- min seq -> int
- sum seq -> int
- exists seq (int -> bool) -> bool
- forall seq (int -> bool) -> bool
- BinOp int int -> bool
- sets of integers?

let ev = 
  let s = ref (nil,0) in
  let rec rec_assert d = ...
  fun d -> s := (d, s); rec_assert s

let main d =
  let s = ref 0 in
  let f1 x = s := !s + x ; x
  let f2 x = s := !s - x ; s
  in begin
    f1 5
    f2 10
    f1 d
    f2 (d - 1)
  end


*)


let apply (af:int -> int) (ax:int) = af ax

let twice (tx:int) = 2 * tx

let neg_twice (nx:int) = 0 - (2 * nx)

let main (n:int(*-:{v:Int | true}*)) = 
    let res =
        if n >= 0 then
            apply twice n
        else 
            apply neg_twice n
    in
    assert(res >= 0)