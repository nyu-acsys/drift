

let main (n(*-:{v:Int | v = 3}*)) (m(*-:{v:Bool | false}*)) = 
	let f x z = 
		if z then x else 2 in

	let id k = k in 

	let res = f (id n) m in
	assert(res = 3)


(*
{v:Int | v = 0} Assign with a const
{v:Int | true} top Int value
{v:Int | v = x} Assign with a relational constraint

TODO:
{v:Bool | true} true Bool value
{v:Bool | false} false Bool value
{v:Bool | either} top Bool value
*)