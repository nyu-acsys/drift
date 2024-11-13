(* CPS conversion. Source Program: 

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
	  else (3, (start, carries,size));

IniCfg = (0, (0, 0));

assert = fun (q, (start, carries, size)) -> q < 3;

assertFinal = fun (q, (start, carries, size)) -> size = prefh - carries + 1;  



*)

let main prefj prefh = 
  let ev = fun k0 q carries size evx ->
             if (q = 0) then k0 1 carries evx () 
             else if ((q = 1) && (evx = 1)) then k0 q (carries + 1) size () 
                  else if ((q = 1) && (evx = 2)) then k0 q carries (size - 1) () 
                       else if ((q = 1) && (evx = 3)) then k0 2 carries (size + 1) () 
                            else k0 3 start carries size () in 
  let ev_assert = fun k1 q0 carries0 size0 x0 ->
                    let k32 q carries size x30 =
                      let x32 = 3 in 
                      let x31 = q < x32 in  let x29 = () in 
                                            assert(x31);k1 q carries size x29 in 
                    ev k32 q0 carries0 size0 x0 in 
  let q1 = 0 in 
  let carries1 = 0 in 
  let size1 = 0 in 
  let f0 = fun k4 q3 carries3 size3 link ->
             let f1 = fun k6 q5 carries5 size5 insTree ->
                        let f2 = fun k8 q7 carries7 size7 _main ->
                                   let k10 q9 carries9 size9 res4 =
                                     let k9 q8 carries8 size8 res3 =
                                       k8 q8 carries8 size8 res3 in 
                                     res4 k9 q9 carries9 size9 prefj in 
                                   _main k10 q7 carries7 size7 prefh in 
                        let f3 = fun k11 q10 carries10 size10 h ->
                                   let f4 = fun k12 q11 carries11 size11 j ->
                                              let x3 = () in 
                                              let k13 q12 carries12 size12 x2 =
                                                let x4 = 0 in 
                                                let k16 q15 carries15 size15 res7 =
                                                  let k15 q14 carries14 size14 res6 =
                                                    let k14 q13 carries13 size13 res5 =
                                                      let x1 = x3 ; res5 in  k12 q13 carries13 size13 x1 in 
                                                    res6 k14 q14 carries14 size14 h in 
                                                  res7 k15 q15 carries15 size15 j in 
                                                insTree k16 q12 carries12 size12 x4 in 
                                              ev_assert k13 q11 carries11 size11 h in 
                                   k11 q10 carries10 size10 f4 in 
                        let k7 q6 carries6 size6 res2 =
                          k6 q6 carries6 size6 res2 in 
                        f2 k7 q5 carries5 size5 f3 in 
             let rec insTree k17 q16 carries16 size16 i =
               let f5 = fun k18 q17 carries17 size17 k ->
                          let f6 = fun k19 q18 carries18 size18 l ->
                                     let x5 = i >= k in 
                                     let k20 q19 carries19 size19 res8 =
                                       k19 q19 carries19 size19 res8 in 
                                     let k21 q20 carries20 size20 res9 =
                                       let x15 = 3 in 
                                       let x14 = () in 
                                       let k28 q27 carries27 size27 x13 =
                                         let x17 = 1 in 
                                         let x16 = l + x17 in  let x12 = x14 ; x16 in  k20 q27 carries27 size27 x12 in 
                                       ev_assert k28 q20 carries20 size20 x15 in 
                                     let k22 q21 carries21 size21 res10 =
                                       let x9 = 2 in 
                                       let x8 = () in 
                                       let k23 q22 carries22 size22 x7 =
                                         let k27 q26 carries26 size26 res14 =
                                           let k26 q25 carries25 size25 res13 =
                                             let k25 q24 carries24 size24 res12 =
                                               let x11 = 1 in 
                                               let x10 = l - x11 in  let k24 q23 carries23 size23 res11 =
                                                                       let x6 = x8 ; res11 in  k20 q23 carries23 size23 x6 in 
                                                                     res12 k24 q24 carries24 size24 x10 in 
                                             res13 k25 q25 carries25 size25 k in 
                                           insTree k26 q26 carries26 size26 res14 in 
                                         link k27 q22 carries22 size22 i in 
                                       ev_assert k23 q21 carries21 size21 x9 in 
                                     if x5 then k21 q18 carries18 size18 x5 else k22 q18 carries18 size18 x5 in 
                          k18 q17 carries17 size17 f6 in 
               k17 q16 carries16 size16 f5 in 
             let k5 q4 carries4 size4 res1 =
               k4 q4 carries4 size4 res1 in 
             f1 k5 q3 carries3 size3 insTree in 
  let f7 = fun k29 q28 carries28 size28 r ->
             let x21 = 1 in 
             let x20 = () in 
             let k30 q29 carries29 size29 x19 =
               let x23 = 1 in 
               let x22 = r + x23 in  let x18 = x20 ; x22 in  k29 q29 carries29 size29 x18 in 
             ev_assert k30 q28 carries28 size28 x21 in 
  let k3 q2 carries2 size2 res0 =
    let k31 q carries size x24 =
      let x27 = prefh - carries in  let x28 = 1 in 
                                    let x26 = x27 + x28 in  let x25 = size = x26 in  assert(x25);x24 in 
    k31 q2 carries2 size2 res0 in 
  f0 k3 q1 carries1 size1 f7