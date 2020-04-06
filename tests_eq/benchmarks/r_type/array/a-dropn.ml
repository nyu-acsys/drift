(*
Drop every i'th element from origin array
*)

let make_array (n:int) (i:int) = assert (0 <= i && i < n); 0
let update (i:int) (n:int) a (x:int) = 
	a i;
  	let ap j = assert (0 <= i && i < n); if i = j then x else a j in ap

let rec drop cai can cbi cbn cdi cdn ca cb = 
	if cai < can && cbi < cbn then 
		if cdi < cdn then
			(let cbp = update cbi cbn cb (ca cai) in
			drop (cai + 1) can (cbi + 1) cbn (cdi + 1) cdn ca cbp)
		else drop (cai + 1) can cbi cbn 0 cdn ca cb
	else cb

let modu n i = n - (n / i * i)

let main = 
	let n = 30 in
	let i = 4 in
	let a = make_array n in
	let j = n - (n - (modu n i)) / i in
	let b = make_array j in
	let bp = drop 0 n 0 j 0 i a b in
	bp(0); bp(j - 1); ()