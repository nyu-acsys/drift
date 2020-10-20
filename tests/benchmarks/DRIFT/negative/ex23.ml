

let rec loop (ly:int) (lz:int) (lc:int) = 
    if lc < 36 then
        if lz >= 0 && lz < 4608 then
            loop ly (lz+1) (lc+1)
        else -1
    else lz

let main (y(*-:{v:Int | true}*)) = 
    if y < 0 then
	    (let c = 0 in
	    let z = y * 36 in
	    let ans = loop y z c in
	    assert(ans >= 0 && ans <= 4608))
	else ()