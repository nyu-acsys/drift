let ev d _s = _s + d

let nondet (i:int) = (i * i * i * 500) mod 199

let rec help (i:int) (length:int) (_s0:int) =
  if i < length then
    let _s1 = ev 1 _s0 in
    let _s2 = ev 1 _s1 in
    if nondet i > 100 then
      let _s3 = ev 1 _s2 in
      let _s4 = ev 1 _s3 in
      help (i+1) length _s4
    else
      let _s5 = ev 1 _s2 in
      help (i+1) length _s5
  else
    _s0

let fill (length:int) =
  let _s00 = 0 in
  let _s01 = ev 1 _s00 in
  help 0 length _s01
  (** breaking version **)
  (* let _s0 = 0 in
  let _s1 = ev 1 _s0 in
  help 0 length _s1 *)

let main (size:int (*-:{v:Int | true}*)) =
  if size > 0 then
    let final = fill size in
    assert false
  else
    ()
