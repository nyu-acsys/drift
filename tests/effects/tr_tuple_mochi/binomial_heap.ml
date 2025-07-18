(*
Tuple Encoding of Product Program.

Source Program: 

(* Abstraction of the insert operation for a Binomial Heap, where
 *  
 *  - link (linking two trees of same rank r) 
 *     * r: rank of the binomial trees
 *  - insTree (stepping through the tree until a missing rank is found)
 *     * i: rank of the current tree to be added to the heap
 *     * k: the first missing rank in the heap
 *     * l: the size of the heap (=number of binomial trees)
 *  - insert (appending the new element to the heap) - represented by main function
 *     * h: the initial size of the heap
 *     * j: the first missing rank in the heap
 *
 * events: link(1); remove(2); append(3) 
 * 
 * Physicist method for amortized cost analysis defines the potential as the size of the heap.
 * Following an insert operation, the potential decreases with the number of linkings performed
 * leading to a constant amortized cost
 * 
 * property:phi(d_i) = phi(d_{i-1}) - #links + 1; 
 *)

let rec link r = ev 1; (r+1)

let rec insTree i k l =
  if i >= k then begin ev 3; (l + 1) end
  else begin ev 2; insTree (link i) k (l-1) end

let main (h:int(*-:{v:Int | v >= 0 }*)) (j:int(*-:{v:Int | v >= 0 }*)) =
  ev h; insTree 0 j h


Property: 

(* 
 * To prove that insert operation runs in O(1) amortized cost, the physicist method use a potential defined 
 * as the number of elements in the heap. Then, the insert operation of a binomial heap  
 * decreases the potential with the number of carried(linked) elements, which approximately describes the number of
 * recursive calls the insert operation takes. 
 * 
 * events: link(1); remove(2); append(3)
 * 
 * Automaton property:
 * (q0)--[v;size:=v]-->(q1)
 * (q1)--[link;carries++]-->(q1)
 * (q1)--[remove;size--]-->(q1)
 * (q1)--[append;size++]-->(q2)
 *)

QSet = [0;1;2;3];
 
delta = fun evx (q, (carries, size)) ->
          if (q = 0) then (1, (carries, evx)) 
      	  else if (q = 1 && evx = 1) then (q, (carries+1, size))
	  else if (q = 1 && evx = 2) then (q, (carries, size-1))
	  else if (q = 1 && evx = 3) then (2, (carries, size+1))
	  else (3, (carries,size));

IniCfg = (0, (0, 0));

assert = fun (q, (carries, size)) -> q < 3;

assertFinal = fun (q, (carries, size)) -> size = prefh - carries + 1;  



*)

let main (prefh:int(*-:{cur_v:Int | cur_v = 0}*)) (prefj:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (carries,size) -> (if (q = 0) then (1,(carries,evx))
                                  else
                                    (if ((q = 1) && (evx = 1)) then (q,((carries + 1),size))
                                    else
                                      (if ((q = 1) && (evx = 2)) then (q,(carries,(size - 1)))
                                      else
                                        (if ((q = 1) && (evx = 3)) then (2,(carries,(size + 1)))
                                        else (3,(carries,size))))))))
in

let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (carries,size) -> assert (q < 3)))
in

let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc2) -> (match acc2 with 
                (carries,size) -> assert (size = ((prefh - carries) + 1))))
in

let link r cfg3 =
  (match let cfg4 = ((ev_step0 1) cfg3) in 
         ((ev_step_asst0 cfg4),cfg4) with 
   (x0,cfg5) -> ((x0 ; (r + 1)),cfg5))
in

let rec insTree i k l cfg6 =
  (if
     (i >= k)
     then
     (match let cfg13 = ((ev_step0 3) cfg6) in 
            ((ev_step_asst0 cfg13),cfg13) with 
      (x6,cfg14) -> ((x6 ; (l + 1)),cfg14))
     else
       (match let cfg7 = ((ev_step0 2) cfg6) in 
              ((ev_step_asst0 cfg7),cfg7) with 
        (x1,cfg8) -> (match (match (match ((link i) cfg8) with 
                                    (x2,cfg9) -> ((insTree x2),cfg9)) with 
                                   (x3,cfg10) -> ((x3 k),cfg10)) with 
                             (x4,cfg11) -> ((x4 (l - 1)) cfg11)))) 
in

  (match (match let cfg16 = ((ev_step0 prefh) (0,(0,0))) in 
                ((ev_step_asst0 cfg16),cfg16) with 
          (x7,cfg17) -> ((((insTree 0) prefj) prefh) cfg17)) with 
         (e0,cfg15) -> ((asst_final0 cfg15) ; e0))
