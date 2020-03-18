

let main (n(*-:{v:Int | v >= 0}*)) =
    let rec map x = (* x <= 3000 *)
      if x = 0
      then 0
      else 1 + map (x - 1) in (* x < 0 && 0 < x <= 3000 *)

    assert (map n = n)
(* in
main 3000  *)
