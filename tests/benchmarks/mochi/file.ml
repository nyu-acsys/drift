(*
USED: PLDI2011 as r-file
USED: PEPM2013 as r-file
KEYWORD: resource
*)

let rec loop x = loop () in
let init = 0 in
let opened = 1 in
let closed = 2 in
let ignore = 3 in
let readit rst =
  if rst = opened then opened else (if rst = ignore then rst else (assert(false); -1))
in
let read_ rx_ rst_ =
  if rx_ then readit rst_ else rst_
in
let closeit cst =
  if cst = opened then closed else (if cst = ignore then cst else (loop (); 0))
in
let close_ cx_ cst_ =
  if cx_ then closeit cst_ else cst_
in
let rec f fx fy fst =
  close_ fy (close_ fx fst); f fx fy (read_ fy (read_ fx fst))
in
let next nst = if nst = init then opened else ignore
in
let g gb3 gx gst = if gb3 > 0 then f gx true (next gst) else f gx false gst
in
let main b2 b3 = if b2 > 0 then g b3 true opened else g b3 false init
in main 1 2 (* bot *)
