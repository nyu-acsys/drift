(* Eratosthene's sieve *)

(* list_interval min max = [min; min+1; ...; max-1; max] *)
let rec list_interval min max =
  if min > max then [] 
	else min :: list_interval (min + 1) max
(*withtype {min:int} int (min) ->
         {max:int | min <= max+1} <max-min+1> =>
         int(max) -> int list(max-min+1)*)

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)
let rec filter p flst = 
	match flst with
		| [] -> []
		| (x::xs') -> 
			if p x then x :: (filter p xs') 
			else filter p xs'
(*withtype ('a -> bool) ->
         {n:nat} <n> => 'a list(n) -> [n':nat | n' <= n] 'a list(n')*)

(* The sieve itself *)
let rec sieve smax lst =
  match lst with
    [] -> []
  | d :: ds -> d :: (sieve smax (filter (fun k -> k > 0) ds))
(*withtype {max:int | max >= 2} int(max) ->
         {l:nat} <l> => int list(l) -> [l':nat | l' <= l] int list(l')*)

let main (m:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if m > 0 && n > m then 
    let xs = list_interval m n in
    let ys = sieve n xs in
    assert (List.length ys <= List.length xs)
  else ()
(*withtype {max:int | max >= 2} <> => int(max) -> int list*)