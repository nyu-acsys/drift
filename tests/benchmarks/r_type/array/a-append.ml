
let make_array (n:int) (i:int) = assert (0 <= i && i < n); 0
let update (i:int) (n:int) a (x:int) = 
	a i;
  	let ap j = assert (0 <= i && i < n); if i = j then x else a j in ap

let rec append_helper hi hn hla ha hb hc = 
	if (hi < hn) then
		let hc2 = 
			if hi < hla then
				update hi hn hc (ha(hi))
			else
				update hi hn hc (hb (hi - hla))
		in
		append_helper (hi + 1) hn hla ha hb hc2
	else hc

let rec append ma mb mn =
	let lc = mn + mn in
	let mc = make_array lc in
	append_helper 0 lc mn ma mb mc

let main (n:int) (k:int) =
	let a = make_array n in
	let b = make_array n in
	let c = append a b n in
	if k >= 0 && k < n + n then
		(c(k); ())
	else ()

let _ = main 3 4