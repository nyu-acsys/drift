
let rec loop (lx:int) (ly:int) (ln:int) = 
        let ny = 
            if (lx <= ln) then ly + 1 
            else if (lx >= ln + 1) then ly - 1 
            else (-4) 
        in
        if (ny < 0) then 
            if ln >= 0 then 
                if ny = (-1) then (lx < 2 * ln + 3)
                else true
            else true
        else loop (lx+1) ny ln 

let main (n:int(*-:{v:Int | true}*)) =
    let x:int = 0 in
    let y:int = 0 in
    if (n >= 0) then assert(loop x y n = true)
    else ()