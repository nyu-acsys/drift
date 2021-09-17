event ev v (d: int) = v := v + d 

let ev d = ()

let ev =
  let s = ref 0 in
  fun d -> s := !s + d; assert (!s >= 0)

let nondet () : bool = ...

let rec reenter d =
   if d > 0 then begin
     ev 1;
     if nondet () then begin 
         reenter (d - 1); 
         ev (-1); 
        end
     else skip
    end

let main (d:int) = reenter d; ev (-1);

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