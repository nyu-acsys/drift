(* A program emits an event every alternate iteration of f.
  It makes sure that the overall number of effects is smaller
  than half of m. *)

let main (m:int(*-:{v:Int | v>0}*)) =
  let flip x =
    if x = 1 then 0
    else 1
  in
  let rec f m1 flag =
    if m1 = 0 then ()
    else
      (if flag=1 then begin ev 1; f (m1-1) end
      else f (m1-1))
      (flip flag)
  in
  f m 0
