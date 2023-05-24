open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)
let rec fold f a l = match l with 
[] -> a
| h::t -> fold f (f a h) t

let count_occ lst target =
fold (fun ack y -> if y = target then ack + 1 else ack) 0 lst

let uniq lst =
fold (fun lst1 y -> if (count_occ lst1 y > 0) then lst1 else y::lst1) [] lst

let rec eval_sig cond lst = match cond with
Empty_String -> lst
| Char c -> c::lst
| Union (c1, c2) -> (eval_sig c1 lst)@(eval_sig c2 lst)
| Concat (c1, c2) -> (eval_sig c1 lst)@(eval_sig c2 lst)
| Star (c1) -> (eval_sig c1 lst)

let rec eval_qs cond curr = match cond with
| Empty_String -> 0::(curr + 1)::[]
| Char c -> 0::(curr + 1)::[]
| Union (c1, c2) -> curr::(eval_qs c1 curr)@(eval_qs c2 (curr +1))
| Concat (c1, c2) -> curr::(eval_qs c1 curr)@(eval_qs c2 (curr+1))
| Star (c1) -> eval_qs c1 curr

let rec eval_fs cond curr = match cond with
| Empty_String -> (curr + 1)::[]
| Char c -> (curr + 1)::[]
| Union (c1, c2) -> (eval_fs c1 curr)@(eval_fs c2 (curr + 1))
| Concat (c1, c2) -> 
let res = fold (fun x y -> x + y) 0 (eval_fs c1 (curr)) in (eval_fs c2 (res - 1))
| Star (c1) -> eval_qs c1 curr

let rec eval_delta cond curr next = match cond with
Empty_String -> (curr, None, next + 1)::[]
| Char c -> (curr, (Some c), next + 1)::[]
| Union (c1, c2) -> (eval_delta c1 curr next)@(eval_delta c2 curr (next + 1))
| Concat (c1, c2) -> 
let ione = (eval_delta c1 curr next) in 
let icurrl = match ione with 
h::t -> h
| _ -> (-1), (Some 'z'), 100
in
let icurr = match icurrl with
st, (Some c), en -> en
|_ -> (-1)
in
ione@(eval_delta c2 icurr icurr)
| Star (c1) -> (0, (Some 'y'), 0)::[]

let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
{
sigma = uniq (eval_sig regexp []);
qs = uniq (eval_qs regexp 0);
q0 = 0;
fs = uniq (eval_fs regexp 0);
delta = eval_delta regexp 0 0
}

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
