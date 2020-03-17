
let main (mn(*-:{v:Int | v >= 0}*)) = 
	let rec fib n = 
		if n <= 1 then 1 else fib (n - 1) + fib (n - 2)
	in
	assert(fib mn >= mn)