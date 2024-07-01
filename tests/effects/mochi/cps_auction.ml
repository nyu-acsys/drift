(* CPS conversion. Source Program: 

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

let main (u: unit(*-:{v: Unit | unit}*)) =
  bid 0


Property: 

(* 
 * Abstraction of an auction smart contract running on blockchain:
 * "The property states that if N users place a bid and the auction has ended, then there will be N-1 successful refunds."
 * 
 * encoding of events: bid(1), close(2), refund(3)
 * 
 * Automaton property
 *   keep track of number of bids 
 * 
 * (q0)--[bid; bidders++]-->(q0)
 * (q0)--[close]-->(q1)
 * (q1)--[refund; refunds--]-->(q1)
 *)

QSet = [0; 1; 2];

delta = fun evx (q, (bidders, refunds)) ->
          if (q = 0 && evx = 1) then (q, (bidders+1, refunds))
	  else if (q = 0 && evx = 2) then (1, (bidders, refunds))
	  else if (q = 1 && evx = 3) then (1, (bidders, refunds+1))
	  else (2, (bidders, refunds));

IniCfg = (0, (0,0));

assert = fun (q, (bidders, refunds)) -> q < 2;

assertFinal = fun (q, (bidders, refunds)) -> (q = 0) || (q = 1 && refunds = bidders - 1);


*)

let main prefu = 
  let ev = fun k0 q bidders refunds evx ->
             if ((q = 0) && (evx = 1)) then k0 q (bidders + 1) refunds () 
             else if ((q = 0) && (evx = 2)) then k0 1 bidders refunds () 
                  else if ((q = 1) && (evx = 3)) then k0 1 bidders (refunds + 1) () 
                       else k0 2 bidders refunds () in 
  let ev_assert = fun k1 q0 bidders0 refunds0 x0 ->
                    let k35 q bidders refunds x39 =
                      let x41 = 2 in 
                      let x40 = q < x41 in  let x38 = () in 
                                            assert(x40);k1 q bidders refunds x38 in 
                    ev k35 q0 bidders0 refunds0 x0 in 
  let q1 = 0 in 
  let bidders1 = 0 in 
  let refunds1 = 0 in 
  let f0 = fun k4 q3 bidders3 refunds3 refund ->
             let f1 = fun k6 q5 bidders5 refunds5 close ->
                        let f2 = fun k8 q7 bidders7 refunds7 bid ->
                                   let f3 = fun k10 q9 bidders9 refunds9 _main ->
                                              let k11 q10 bidders10 refunds10 res4 =
                                                k10 q10 bidders10 refunds10 res4 in 
                                              _main k11 q9 bidders9 refunds9 prefu in 
                                   let f4 = fun k12 q11 bidders11 refunds11 u ->
                                              let x1 = 0 in 
                                              let k13 q12 bidders12 refunds12 res5 =
                                                k12 q12 bidders12 refunds12 res5 in 
                                              bid k13 q11 bidders11 refunds11 x1 in 
                                   let k9 q8 bidders8 refunds8 res3 =
                                     k8 q8 bidders8 refunds8 res3 in 
                                   f3 k9 q7 bidders7 refunds7 f4 in 
                        let rec bid k14 q13 bidders13 refunds13 i =
                          let x2 = Random.int(0) in 
                          let x3 = 0 in 
                          let bool_random0 = x2 >= x3 in 
                          let k15 q14 bidders14 refunds14 res6 =
                            k14 q14 bidders14 refunds14 res6 in 
                          let k16 q15 bidders15 refunds15 res7 =
                            let x7 = 1 in 
                            let x6 = () in 
                            let k20 q19 bidders19 refunds19 x5 =
                              let x9 = 1 in 
                              let x8 = i + x9 in  let k21 q20 bidders20 refunds20 res11 =
                                                    let x4 = x6 ; res11 in  k15 q20 bidders20 refunds20 x4 in 
                                                  bid k21 q19 bidders19 refunds19 x8 in 
                            ev_assert k20 q15 bidders15 refunds15 x7 in 
                          let k17 q16 bidders16 refunds16 res8 =
                            let k19 q18 bidders18 refunds18 res10 =
                              k15 q18 bidders18 refunds18 res10 in 
                            close k19 q16 bidders16 refunds16 i in 
                          if bool_random0 then k16 q13 bidders13 refunds13 bool_random0 else k17 q13 bidders13 refunds13 bool_random0 in 
                        let k7 q6 bidders6 refunds6 res2 =
                          k6 q6 bidders6 refunds6 res2 in 
                        f2 k7 q5 bidders5 refunds5 bid in 
             let f5 = fun k22 q21 bidders21 refunds21 n ->
                        let x11 = 0 in 
                        let x10 = n = x11 in 
                        let k23 q22 bidders22 refunds22 res12 =
                          k22 q22 bidders22 refunds22 res12 in 
                        let k24 q23 bidders23 refunds23 res13 =
                          let x18 = () in 
                          k23 q23 bidders23 refunds23 x18 in 
                        let k25 q24 bidders24 refunds24 res14 =
                          let x15 = 2 in 
                          let x14 = () in 
                          let k26 q25 bidders25 refunds25 x13 =
                            let x17 = 1 in 
                            let x16 = n - x17 in  let k27 q26 bidders26 refunds26 res15 =
                                                    let x12 = x14 ; res15 in  k23 q26 bidders26 refunds26 x12 in 
                                                  refund k27 q25 bidders25 refunds25 x16 in 
                          ev_assert k26 q24 bidders24 refunds24 x15 in 
                        if x10 then k24 q21 bidders21 refunds21 x10 else k25 q21 bidders21 refunds21 x10 in 
             let k5 q4 bidders4 refunds4 res1 =
               k4 q4 bidders4 refunds4 res1 in 
             f1 k5 q3 bidders3 refunds3 f5 in 
  let rec refund k28 q27 bidders27 refunds27 j =
    let x20 = 0 in 
    let x19 = j <= x20 in 
    let k29 q28 bidders28 refunds28 res16 =
      k28 q28 bidders28 refunds28 res16 in 
    let k30 q29 bidders29 refunds29 res17 =
      let x27 = () in 
      k29 q29 bidders29 refunds29 x27 in 
    let k31 q30 bidders30 refunds30 res18 =
      let x24 = 3 in 
      let x23 = () in 
      let k32 q31 bidders31 refunds31 x22 =
        let x26 = 1 in 
        let x25 = j - x26 in  let k33 q32 bidders32 refunds32 res19 =
                                let x21 = x23 ; res19 in  k29 q32 bidders32 refunds32 x21 in 
                              refund k33 q31 bidders31 refunds31 x25 in 
      ev_assert k32 q30 bidders30 refunds30 x24 in 
    if x19 then k30 q27 bidders27 refunds27 x19 else k31 q27 bidders27 refunds27 x19 in 
  let k3 q2 bidders2 refunds2 res0 =
    let k34 q bidders refunds x28 =
      let x31 = 0 in 
      let x30 = q = x31 in 
      let x34 = 1 in 
      let x33 = q = x34 in  let x37 = 1 in 
                            let x36 = bidders - x37 in  let x35 = refunds = x36 in  let x32 = x33 && x35 in  let x29 = x30 || x32 in  assert(x29);x28 in 
    k34 q2 bidders2 refunds2 res0 in 
  f0 k3 q1 bidders1 refunds1 refund