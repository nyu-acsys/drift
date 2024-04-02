
let rec shrink t f d =
  if f () <= 0 then 0 
  else begin
    ev 1; (* ev[Shrink] *)
    let t' = f() - d in
    shrink t' (fun x -> t') d end

let main (gl_t:int(*-:{v:Int | true}*)) (gl_d:int(*-:{v:Int | true}*)) =   
    ev (gl_t/gl_d);
    shrink gl_t (fun x -> gl_t) gl_d
