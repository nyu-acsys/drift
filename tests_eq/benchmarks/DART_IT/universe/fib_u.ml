

let rec fib n = 
	if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let main (mn(*-:{v:Int | v >= 0}*)) = 
	let ans = if mn >= 0 then
		assert(fib mn >= mn)
	else assert(true)

let _ = main 5