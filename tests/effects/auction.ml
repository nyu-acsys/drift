
(* 

====== VER 1 =========================

let rec refund j =
  if j <= 0 then ()  
  else begin ev 3; refund (j - 1) end

let close n = 
  if n=0 then () 
  else begin ev 2; refund (n-1) end

let rec bid i =
  if nondet then
    begin ev 1; bid (i + 1) end
  else
    close i

======= END VER 1 ====================


======= VER 2 ======================================================================================

let refund k amt h () = 
  if k <= 0 then ()
  else begin (Format.printf "ev 3 [refund]: (%d, %d)\n" k amt); h () end

let close j g = 
  if j = 0 then ()
  else begin (Format.printf "ev 2 [close]\n" ); g () end 

let rec bid i amtW f =
  let amt = amtW + 1 in
  if i < 3 then 
    begin (Format.printf "ev 1 [bid]: (%d, %d)\n" (i+1) amt); bid (i + 1) amt (refund i amtW f) end
  else 
    close i f

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 0 1 (fun () -> ())

let () = main ()

======= VER 2 END ==================================================================================
*)

(* let refund k amt h () =  *)
(*   if k <= 0 then () *)
(*   else begin ev 3; h () end *)

(* let close j g =  *)
(*   if j = 0 then () *)
(*   else begin ev 2; g () end  *)

(* let rec bid i amtW f = *)
(*   let amt = amtW + 1 in *)
(*   if i < 3 then  *)
(*     begin ev 1; bid (i + 1) amt (refund i amtW f) end *)
(*   else  *)
(*     close i f *)
(* let id () = () *)

let refund k kamt h _ = 
  if k <= 0 then ()
  else begin ev 3; h () end

let close j g = 
  if j = 0 then ()
  else begin ev 2; g () end

let rec bid i amtW f =
  let amt = amtW + 1 in
  if nondet then 
    begin ev 1; bid (i + 1) amt (refund i amtW f) end
  else
    close i f

let idf _ = ()

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 0 1 idf
