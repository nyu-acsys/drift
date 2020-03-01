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
    else ()
in

let main n0 n1 n2 =
    if (n0 >= 0 && n1 >= 0 && n2 >= 0) then
        let i = 0 in
        let k = 0 in
        let a1 = loopa i k n0 in
        let a2 = loopa i a1 n1 in
        let a3 = loopa i a2 n2 in
        let b1 = loopb i a3 n2 in
        let b2 = loopb i b1 n1 in 
        loopc i b2 n0 
    else ()
in main 2 3 4