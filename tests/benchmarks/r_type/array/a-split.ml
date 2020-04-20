
let make_array (n:int) (i:int) = assert (0 <= i && i < n); 0
let update (i:int) (n:int) a (x:int) = 
	a i;
  	let ap j = assert (0 <= i && i < n); if i = j then x else a j in ap

let rec copy cai can cbi cbn (ca:int -> int) (cb:int -> int) = 
	if cai < can && cbi < cbn then 
		(let cbp = update cbi cbn cb (ca cai) in
		copy (cai + 1) can (cbi + 1) cbn ca cbp)
	else cb

let main (n:int) (i:int) = 
	let a = make_array n in
	if i >= 0 && i < n then
		let b = make_array i in
		let bp = copy 0 n 0 i a b in
		let c = make_array (n - i) in
		let cp = copy i n 0 (n - i) a c in
		bp 0; bp (i - 1); cp 0; cp (n - i - 1); ()
	else
		()