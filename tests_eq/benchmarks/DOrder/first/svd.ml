
let rec loopa ai aj al am an = 
    if aj <= an then
        (1<=aj &&
        aj<=an &&
        1<=ai &&
        ai<=an &&
    (*//assert(1<=i);assert(i<=m); // TODO feasible counterexample found*)
        1<=al &&
        al<=an &&
        loopa ai (aj+1) al am an)
    else true

let rec loopc bi bj bk bl bm bn = 
    if bk <= bn then 
        ((*//assert(1<=i);assert(i<=m); // TODO feasible counterexample found*)
        1<=bk && bk<=bn &&
        1<=bj && bj<=bn &&
        loopc bi bj (bk+1) bl bm bn)
    else true     

let rec loopd di dj dk dl dm dn =
    if dk <= dn then 
        (1<=dk && dk<=dn &&
        1<=dj && dj<=dn &&
        1<=di && di<=dn &&
        loopd di dj (dk+1) dl dm dn)
    else true         

let rec loopb ci cj cl cm cn = 
    if cj <= cn then 
        (loopc ci cj cl cl cm cn &&
        loopd ci cj cl cl cm cn &&
        loopb ci (cj+1) cl cm cn)    
    else true

let rec loope ei ej el em en = 
    if ej <= en then 
        (1<=ej && ej<=en &&
        1<=ei && ei<=en &&
        loope ei (ej+1) el em en)
    else true

let rec loop i l m n = (* Accumulation of right-hand transformations. *)
    if (i >= 1) then    
        ((if (i < n) then (
            loopa i l l m n && (* Double division to avoid possible underflow. *)
            loopb i l l m n &&
            loope i l l m n
        ) else true )
        && 1<=i && i<=n
        && loop (i - 1) i m n)
    else true

let main (ml:int) (mm:int) (mn:int) =
    if (ml > 0) then 
        assert(loop mn ml mm mn = true)
    else ()
        
let _ = main 4 0 3
let _ = main 3 2 4
let _ = main (-3) 2 4
let _ = main 3 (-2) (-5)
let _ = main 69 (-10) 20
let _ = main 3 2 (-4)
