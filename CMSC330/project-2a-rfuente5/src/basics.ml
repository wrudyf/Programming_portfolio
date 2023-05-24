open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b, c) =
(c, b, a)

let is_even x = if x mod 2 = 0 then true else false

let area (a, b) (x, y) =
let t1 = a - x in
let t2 = b - y in
let t3 = t1 * t2 in
if t3 < 0 then t3 * -1 else t3


(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
match n with
0 -> 0
| 1 -> 1
| _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec pow x p =
if p = 1 then x else x * pow x (p - 1)

let rec aux_prime x fact_count num =
if num = x then (if fact_count > 2 then false else true) else (if x mod num = 0 then aux_prime x (fact_count + 1) (num +1) else aux_prime x fact_count (num + 1))

let is_prime x =
if x = 0 then false else
if x < 0 then false else (if x = 2 then true else (if x = 1 then false else aux_prime x 1 1))
			     
let rec maxFuncAux init funcs = match funcs with
[] -> []
|h::t ->
let applied_val = (h init) in
(init::applied_val::maxFuncAux init t)@(init::applied_val::maxFuncAux applied_val t)

let rec findMax lst v = match lst with
[] -> v
| h::t -> if v < h then findMax t h else findMax t v

let rec maxFuncChain init funcs = 
let lst1 = maxFuncAux init funcs in
match lst1 with
[] -> init
| h::t-> findMax lst1 h

(*****************)
(* Part 3: Lists *)
(*****************)

let rec aux_reverse lst elst = match lst, elst with
[], h::t -> elst
| h::t, [] -> aux_reverse (t) (h::[])
| h1::t1, h2::t2 -> aux_reverse (t1) (h1::h2::t2)
| _, _ -> []

let reverse lst = aux_reverse lst []

let rec size lst = match lst with
[] -> 0
| h::t -> 1 + (size t)

let rec bubblesort lst = match lst with
[] -> []
| h1::h2::t ->
if h1 > h2 then h2::(bubblesort (h1::t)) else h1::(bubblesort (h2::t))
|h::[] -> lst

let rec do_sort lst runs =
let list_size = (size lst) in
if runs = list_size * list_size then lst else do_sort (bubblesort lst) (runs + 1)

let rec merge lst1 lst2 =
let mlst = lst1@lst2 in
do_sort mlst 0

let rec is_palindrome lst =
let lst1 = lst in
let lst2 = (reverse lst) in
if lst1 = lst2 then true else false

let rec aux_jump lst1 lst2 index size = match lst1, lst2 with
[], [] -> []
| oddh::oddt, evenh::event ->
if index mod 2 = 0 then (match evenh with (a,b) -> b::(aux_jump oddt event (index + 1) size))
else (match oddh with (a, b) -> a::(aux_jump oddt event (index + 1) size))
| _, _ -> []

let jumping_tuples lst1 lst2 =
let size1 = size lst1 in 
let size2 = size lst2 in
if size1 = size2 then aux_jump lst1 lst2 0 size1
else if size1 < size2 then aux_jump lst1 lst2 0 size1 else aux_jump lst1 lst2 0 size2


let rec flatten lst = match lst with
[] -> []
| h::t -> h@(flatten t)

let rec square_primes lst = match lst with
[] -> []
| h::t -> if ((is_prime h) = true) then (h, h*h)::square_primes t else square_primes t

let rec check_yes f lst = match lst with
[] -> []
| h::t -> if ((f h) = true) then h::(check_yes f t) else check_yes f t

let rec check_no f lst = match lst with
[] -> []
| h::t -> if ((f h) = false) then h::(check_no f t) else check_no f t

let partition p lst =
let lst_yes = check_yes p lst in
let lst_no = check_no p lst in
(lst_yes, lst_no)

(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x =
map (fun y -> if y = x then 1 else 0) lst

let count_occ lst target =
fold (fun ack y -> if y = target then ack + 1 else ack) 0 lst

let uniq lst =
fold (fun lst1 y -> if (count_occ lst1 y > 0) then lst1 else y::lst1) [] lst
