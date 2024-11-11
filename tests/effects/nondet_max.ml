(* A program that chooses n numbers between 0 and m. 
  Each number is chosen between 0 and m non-deterministically.
  The property file checks if every chosen number is ≤ m. *)

let rec make_list n m f =
  if n = 0 then 0
  else begin
    ev (f m);
    make_list (n-1) (m+1) f
  end
let rec get_random_int n1 =
  if n1 = 0 then 0
  else
    if nondet then get_random_int (n1-1)
    else n1

let main (n:int(*-:{v:Int | v>0}*)) =
  make_list n 0 (get_random_int)
