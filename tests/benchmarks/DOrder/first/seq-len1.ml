(*
Need to test for delay widening
*)


let main (n0(*-:{v:Int | v >= 0}*)) (n1(*-:{v:Int | v >= 0}*)) (n2(*-:{v:Int | v >= 0}*)) =
    let rec loopa ai ak an = 
        if ai < an then loopa (ai+1) (ak+1) an
        else ak 
    in

    let rec loopb bi bk bn = 
        if bi < bn then loopb (bi+1) (bk - 1) bn
        else bk
    in

    let rec loopc ci ck cn = 
        if ci < cn then (assert (ck > 0); loopc (ci+1) (ck - 1) cn)
        else true
    in

    let i = 0 in
    let k = 0 in
    let a1 = loopa i k n0 in
    let a2 = loopa i a1 n1 in
    let a3 = loopa i a2 n2 in
    let b1 = loopb i a3 n2 in
    let b2 = loopb i b1 n1 in 
    assert(loopc i b2 n0 = true)

(* in assert(main 2 3 4 = true) *)