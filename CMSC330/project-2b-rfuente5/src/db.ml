type person = { name: string;
                age: int;
                hobbies: string list }

(* Define the type of db below *)
type db = person list

let newDatabase = []

let insert person db = person::db

let rec remove name db = match db with
[] -> []
| h::t -> if h.name = name then remove name t else (h::(remove name t))

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec evaluate cond v =
match cond with
| True -> true
| False -> false
| Age f -> (f v.age)
| Name f -> (f v.name)
| Hobbies f -> (f v.hobbies)
| And (c1, c2) -> (evaluate c1 v) && (evaluate c2 v)
| Or (c1, c2) -> (evaluate c1 v) || (evaluate c2 v)
| Not c1 -> if (evaluate c1 v = true) then false else true
| If (c1, c2, c3) -> if (evaluate c1 v = true) then (evaluate c2 v) else (evaluate c3 v)

let rec query condition db = match db with
[] -> []
| h::t -> if (evaluate condition h) = true then h::query condition t else
query condition t

type comparator = person -> person -> int

let rec bubblesort comp db = match db with
[] -> []
| h1::h2::t ->
let v1 = (comp h1 h2) in
if v1 = 1 then h2::(bubblesort comp (h1::t)) else h1::(bubblesort comp (h2::t))
| h::[] -> db

let rec size lst = match lst with
[] -> 0
| h::t -> 1 + (size t)

let rec do_sort lst compp runs =
let list_size = (size lst) in
if runs = list_size * list_size then lst else do_sort (bubblesort compp lst) compp (runs + 1)

let rec sort comparator db =
do_sort db comparator 0

let queryBy condition db comparator =
let lst1 = query condition db in
let lst2 = sort comparator lst1 in
lst2

let rec query2 condition db f = match db with
[] -> []
| h::t -> if (evaluate condition h) = true then (f h)::query2 condition t f else h::query2 condition t f

let update condition db personData =
query2 condition db personData

let rec query3 condition db = match db with
[] -> []
| h::t -> if (evaluate condition h) = true then query3 condition t else h::query3 condition t

let deleteAll condition db =
query3 condition db
