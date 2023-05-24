open List
open Sets

(*********)

(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
let rec fold f a l = match l with
[] -> a
| h::t -> fold f (f a h) t

let rec sig_check symbol lst = match lst with
[] -> false
| h::t -> if symbol = h then true else sig_check symbol t

let count_occ lst target =
fold (fun ack y -> if y = target then ack + 1 else ack) 0 lst

let uniq lst =
fold (fun lst1 y-> if (count_occ lst1 y > 0) then lst1 else y::lst1) [] lst

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
match s with
None -> 
let mlst = fold (fun qx qy -> 
let lst = fold (fun x y -> match y with
st0, None, en0 -> if st0 = qy then en0::x else x
| st, Some c2, en -> x
) [] nfa.delta in (uniq lst)@qx

) [] qs in (uniq mlst)

| Some c ->
if (sig_check c nfa.sigma) = false then [] else
let m2lst = fold (fun qx qy -> 
let lst1 = 
fold (fun x y -> match y with
st0, None, en0 -> x
| st, Some c2, en -> if st = qy && c = c2 then en::x else x
) [] nfa.delta in (uniq lst1)@qx

)
[] qs in (uniq m2lst)

let rec eps_trav q0 acc lst = match lst with
[] -> acc
| h::t -> match h with
st, None, en -> 
if q0 = st then (eps_trav q0 (en::acc) t)@(eps_trav en acc lst) else eps_trav q0 acc t
| st, Some c, en -> eps_trav q0 acc t

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
let lst1 = fold (fun qx qy ->
let lst =  eps_trav qy [] nfa.delta
in qy::(uniq lst)@qx
) [] qs in (uniq lst1)

let rec acc_aux nfa clst state = match clst with
[] -> (e_closure nfa state)
| h::t -> acc_aux nfa t ( e_closure nfa (move nfa state (Some h)) )

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
let chlst = (explode s) in 
let states = acc_aux nfa chlst [nfa.q0] in 
let count = fold (fun x y -> 
let num = fold (fun x1 y2 -> if y2 = y then x1 + 1 else x1) 0 states
in x + num
) 0 nfa.fs in if count > 0 then true else false

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
fold (fun x y ->
(e_closure nfa (move nfa qs (Some y)) )::x
) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
fold (fun x y ->
(qs, Some y, (e_closure nfa (move nfa qs (Some y)) ) )::x
) [] nfa.sigma


let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
let count = fold (fun x y ->
(fold (fun x1 y1 ->
if y1 = y then x1 + 1 else x1
) 0 qs) + x
) 0 nfa.fs in if count > 0 then [qs] else []

let compare lst1 lst2 =
let size1 = fold (fun x y -> x + 1) 0 lst1 in
let size2 = fold (fun x y -> x + 1) 0 lst2 in
let count =
fold (fun x y ->
fold (fun x2 y2 -> if y = y2 then x2 + 1 else x2) 0 lst2 + x
) 0 lst1 in 
if size1 = count && size1 = size2 then true else false

let processed lst elem =
let count = fold (fun x y ->
if compare y elem = true then x + 1 else x) 0 lst in
if count = 1 then true else false

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let rec aux_conv visited unvisited nfa dfa = match unvisited with
[[]] -> dfa
| h::t ->
if processed visited h = true then
aux_conv visited t nfa {
sigma = dfa.sigma;
qs = dfa.qs;
q0 = dfa.q0;
fs = dfa.fs;
delta = dfa.delta
}
else
let n_states = new_states nfa h in
let n_trans = new_trans nfa h in
let n_final = new_finals nfa h in
aux_conv (h::visited) (n_states@t) nfa {
sigma = dfa.sigma;
qs = h::dfa.qs;
q0 = dfa.q0;
fs = n_final@dfa.fs;
delta = n_trans@dfa.delta
}
| _ -> dfa
  
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
let init_s = e_closure nfa [nfa.q0] in
aux_conv [] [init_s] nfa {
sigma = nfa.sigma;
qs = [];
q0 = init_s;
fs = [];
delta = []
}
