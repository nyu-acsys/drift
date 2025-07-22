(*
Tuple Encoding of Product Program.

Source Program: 


let succ (sb:int) (sf:int->unit) sx = sf (sx + 1)

let rec app3 (b:int) (f:int->unit) (a:int) (g:int->(int->unit)->unit) (k:int) = 
	if k > 0 then app3 b (succ b f) b g (k - 1) else g b f

let app ax (ab:int) (af:int->unit) = af ax

let check (cx:int) = ev cx

let main (i:int(*-:{v:Int | true}*)) (j:int(*-:{v:Int | true}*)) = 
    ev i;
    app3 i (check) i (app i) j


Property: 

QSet = [0; 1; 2];

delta = fun evx (q, acc) ->
          if (q = 0) then (1, evx)
	  else if (q = 1 && evx >= acc) then (1, acc)
	  else (2, acc);
	  
IniCfg = (0, 0);

assert = fun (q, acc) -> q < 2;


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if (q = 0) then (1,evx)
              else (if ((q = 1) && (evx >= acc)) then (1,acc)
                   else (2,acc))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q < 2))


let succ sb sf sx cfg2 =
  ((sf (sx + 1)) cfg2)


let rec app3 b f a g k cfg3 =
  (if (k > 0) then ((((((app3 b) ((succ b) f)) b) g) (k - 1)) cfg3)
  else (((g b) f) cfg3))


let app ax ab af cfg4 =
  ((af ax) cfg4)


let check cx cfg5 =
  let cfg6 = ((ev_step0 cx) cfg5) in 
  ((ev_step_asst0 cfg6),cfg6) 


let main (i:int(*-:{cur_v:Int | true = true}*)) (j:int(*-:{cur_v:Int | true = true}*)) =
  (match let cfg7 = ((ev_step0 i) (0,0)) in 
         ((ev_step_asst0 cfg7),cfg7) with 
   (x0,cfg8) -> ((((((app3 i) check) i) (app i)) j) cfg8))