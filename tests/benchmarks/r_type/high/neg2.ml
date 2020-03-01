let g gx = 2 * gx in

let twice tx tf = tf (tf tx) in

let neg nx = (0 - nx) in

let main n =
	let z = twice (g n) neg in
	if (n > 0) then assert (z >= 0)
	else assert (z <= 0)
in main 3