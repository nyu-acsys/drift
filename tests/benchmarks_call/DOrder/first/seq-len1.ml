

let rec loopa ai ak an = 
    if ai < an then loopa (ai+1) (ak+1) an
    else ak 

let rec loopb bi bk bn = 
    if bi < bn then loopb (bi+1) (bk - 1) bn
    else bk

let rec loopc ci ck cn = 
    if ci < cn then (assert (ck > 0); loopc (ci+1) (ck - 1) cn)
    else true

let main_p (n0:int) (n1:int) (n2:int) =
    if (n0 >= 0 && n1 >= 0 && n2 >= 0) then
        let i = 0 in
        let k = 0 in
        let a1 = loopa i k n0 in
        let a2 = loopa i a1 n1 in
        let a3 = loopa i a2 n2 in
        let b1 = loopb i a3 n2 in
        let b2 = loopb i b1 n1 in 
        assert(loopc i b2 n0 = true)
    else ()

let main (w:unit) =
	let _ = main_p 2 3 4 in
    let _ = main_p 3 2 4 in
    let _ = main_p (-2) 2 3 in
    let _ = main_p 2 (-2) 3 in
    let _ = main_p 3 2 (-2) in
    (* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()