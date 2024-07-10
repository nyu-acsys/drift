(* Abstraction of the insert operation for a Binomial Heap, where
 *  
 *  - link (linking two trees of same rank r) 
 *     * r: rank of the binomial trees
 *  - insTree (stepping through the tree until a missing rank is found)
 *     * i: rank of the current tree to be added to the heap
 *     * k: the first missing rank in the heap
 *     * l: the size of the heap (=number of binomial trees)
 *  - insert (appending the new element to the heap) - represented by main function
 *     * h: the initial size of the heap
 *     * j: the first missing rank in the heap
 *
 * events: link(1); remove(2); append(3) 
 * 
 * Physicist method for amortized cost analysis defines the potential as the size of the heap.
 * Following an insert operation, the potential decreases with the number of linkings performed
 * leading to a constant amortized cost
 * 
 * property:phi(d_i) = phi(d_{i-1}) - #links + 1; 
 *)

let rec link r = ev 1; (r+1)

let rec insTree i k l =
  if i >= k then begin ev 3; (l + 1) end
  else begin ev 2; insTree (link i) k (l-1) end

let main (h:int(*-:{v:Int | v >= 0 }*)) (j:int(*-:{v:Int | v >= 0 }*)) =
  ev h; insTree 0 j h
