type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with
Leaf -> init
| Node(l, v, r) -> f (tree_fold f init l) v (tree_fold f init r)

let map tree f = 
tree_fold (fun l v r -> Node(l, (f v), r)) Leaf tree

let mirror tree = 
tree_fold (fun l v r -> Node(r, v, l)) Leaf tree

let in_order tree = 
let lst = [] in
tree_fold (fun l v r -> l@ (v::lst)@r) lst tree

let pre_order tree = 
let lst = [] in 
tree_fold (fun l v r -> (v::lst)@l@r) lst tree

(*this compose function took me an annoyingly long amount of time *)
let compose tree = 
tree_fold (fun l v r -> (fun x -> (r (v (l x) ) ) )) (fun x -> x) tree

let max x y = if x > y then x else y

let depth tree = 
tree_fold 
(fun l v r -> 
let v1 = 1 in 
max (l + v1) (r + v1)
)
0 tree

(* Assume complete tree *)
let transform tree = 
let tree_size = depth tree in 
let level = 0 in 
tree_fold (fun l v r ->
let x = depth l in 
Node(l, (level + (tree_size - x), v), r)
)
Leaf tree


let trim tree n = 
let tree1 = transform tree in 
tree_fold (fun l v r -> 
match v with 
(a, b) -> if a = n then Node(Leaf, b, Leaf) else Node(l, b, r)
)
Leaf tree1


let rec get_lists lst v left = match lst with 
[] -> ([], [])
| h::t -> if v = h then (left, t) else (get_lists t v (left@[h]))


let split lst v =
get_lists lst v []

let rec get_first lst = match lst with
[] -> failwith "empty list"
| h::t -> h

let rec get_lists_by_ind lst count ind left = match lst with 
[] -> ([], [])
| h::t -> if count = ind then (left, h::t) else (get_lists_by_ind t (count + 1) ind (left@[h]))

let is_empty lst = match lst with 
| [] -> true
| h::t -> false

let rec get_size lst a = match lst with
[] -> a
| h::t -> get_size t (a + 1)

let get_rest lst = match lst with
[] -> []
| h::t -> t

let rec from pre in_ord acc =
if (is_empty pre) = true then acc else
let first = get_first pre in
let lsts_i = split in_ord first in
match lsts_i with
(a, b) -> let length = (get_size a 0) in
let lsts_p = get_lists_by_ind (get_rest pre) 0 length [] in

match lsts_p with
(c, d) -> Node( (from a c Leaf), first, (from d b Leaf))

let from_pre_in pre in_ord = 
from pre in_ord Leaf
