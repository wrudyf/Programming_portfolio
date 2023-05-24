open Str
open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec rev lst = match lst with
[] -> []
| h::t -> (rev t)@[h]


let rec check s i tok_lst = 
let size = String.length s in 

(*get token for let*)
if (Str.string_match (Str.regexp "let") s i ) = true then
if (i + 3) < size then check s (i + 3) (Tok_Let::tok_lst) else (Tok_Let::tok_lst)

(*get token for def*)
else if (Str.string_match (Str.regexp "def") s i) = true then
if (i + 3) < size then check s (i + 3) (Tok_Def::tok_lst) else (Tok_Def::tok_lst)

(*get token for In*)
else if (Str.string_match (Str.regexp "in") s i) = true then
if (i + 2) < size then check s (i + 2) (Tok_In::tok_lst) else (Tok_In::tok_lst)

(*get token for fun*)
else if (Str.string_match (Str.regexp "fun") s i) = true then 
if (i + 3) < size then check s (i + 3) (Tok_Fun::tok_lst) else (Tok_Fun::tok_lst)

(*get token for Rec*)
else if (Str.string_match (Str.regexp "rec") s i) = true then 
if (i + 3) < size then check s (i + 3) (Tok_Rec::tok_lst) else (Tok_Rec::tok_lst)

(*get token for Arrow*)
else if (Str.string_match (Str.regexp "->") s i) = true then
if (i + 2) < size then check s (i + 2) (Tok_Arrow::tok_lst) else (Tok_Arrow::tok_lst)

(*get token for DoubleSemi*)
else if (Str.string_match (Str.regexp ";;") s i) = true then 
if (i + 2) < size then check s (i + 2) (Tok_DoubleSemi::tok_lst) else (Tok_DoubleSemi::tok_lst)

(*get token for NotEqual*)
else if (Str.string_match (Str.regexp "<>") s i) = true then
if (i + 2) < size then check s (i + 2) (Tok_NotEqual::tok_lst) else
(Tok_NotEqual::tok_lst)

(*get token for LessEqual*)
else if (Str.string_match (Str.regexp "<=") s i) = true then 
if (i + 2) < size then check s (i + 2) (Tok_LessEqual::tok_lst) else (Tok_LessEqual::tok_lst)

(*get token for GreaterEqual*)
else if (Str.string_match (Str.regexp ">=") s i ) = true then 
if (i + 2) < size then check s (i + 2) (Tok_GreaterEqual::tok_lst) else (Tok_GreaterEqual::tok_lst)

(*get token for Equal*)
else if (Str.string_match (Str.regexp "=") s i) = true then 
if (i + 1) < size then check s (i + 1) (Tok_Equal::tok_lst) else (Tok_Equal::tok_lst)

(*get token for Greater*)
else if (Str.string_match (Str.regexp ">") s i) = true then
if (i + 1) < size then check s (i + 1) (Tok_Greater::tok_lst) else (Tok_Greater::tok_lst)

(*get token for Less*)
else if (Str.string_match (Str.regexp "<") s i) = true then 
if (i + 1) < size then check s (i + 1) (Tok_Less::tok_lst) else (Tok_Less::tok_lst)

(*get token for Not*)
else if (Str.string_match (Str.regexp "not") s i) = true then
if (i + 3) < size then check s (i + 3) (Tok_Not::tok_lst) else (Tok_Not::tok_lst)

(*get token for If*)
else if (Str.string_match (Str.regexp "if") s i) = true then
if (i + 2) < size then check s (i + 2) (Tok_If::tok_lst) else (Tok_If::tok_lst)

(*get token for Then*)
else if (Str.string_match (Str.regexp "then") s i) = true then 
if (i + 4) < size then check s (i + 4) (Tok_Then::tok_lst) else (Tok_Then::tok_lst) 

(*get token for Else*)
else if (Str.string_match (Str.regexp "else") s i) = true then
if (i + 4) < size then check s (i + 4) (Tok_Else::tok_lst) else (Tok_Else::tok_lst) 

(*get token for Or*)
else if (Str.string_match (Str.regexp "||") s i) = true then
if (i + 2) < size then check s (i + 2) (Tok_Or::tok_lst) else (Tok_Or::tok_lst)

(*get token for And*)
else if (Str.string_match (Str.regexp "&&") s i) = true then 
if (i + 2) < size then check s (i + 2) (Tok_And::tok_lst) else (Tok_And::tok_lst)

(*get token for Add*)
else if (Str.string_match (Str.regexp "+") s i) = true then
if (i + 1) < size then check s (i + 1) (Tok_Add::tok_lst) else (Tok_Add::tok_lst)

(*get token for Sub*)
else if (Str.string_match (Str.regexp "-") s i) = true then
if (i + 1) < size then check s (i + 1) (Tok_Sub::tok_lst) else (Tok_Sub::tok_lst)

(*get token for Concat*)
else if (Str.string_match (Str.regexp {|\^|}) s i) = true then 
if (i + 1) < size then check s (i + 1) (Tok_Concat::tok_lst) else (Tok_Concat::tok_lst)

(*get token for Mult*)
else if (Str.string_match (Str.regexp "*") s i) = true then
if (i + 1) < size then check s (i + 1) (Tok_Mult::tok_lst) else (Tok_Mult::tok_lst)

(*get token for Div*)
else if (Str.string_match (Str.regexp "/") s i) = true then
if (i + 1) < size then check s (i + 1) (Tok_Div::tok_lst) else (Tok_Div::tok_lst)

(*get token for true bool*)
else if (Str.string_match (Str.regexp "true") s i) = true then
if (i + 4) < size then check s (i + 4) (Tok_Bool(true)::tok_lst) else (Tok_Bool(true)::tok_lst)

(*get token for false bool*)
else if (Str.string_match (Str.regexp "false") s i) = true then
if (i + 5) < size then check s (i + 5) (Tok_Bool(false)::tok_lst) else (Tok_Bool(false)::tok_lst)

(*get token for string *)
else if (Str.string_match (Str.regexp "\"[^\"]*\"") s i) = true then
let size1 = String.length (matched_string s) in 
let sub1 = string_after (matched_string s) 1 in 
let sub2 = string_before sub1 ((String.length sub1) - 1) in  
if (i + size1) < size then check s (i + size1) ((Tok_String(sub2))::tok_lst) else (Tok_String(sub2)::tok_lst)

(*get token for tok id*)
else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") s i) = true then 
let sub1 = matched_string s in
let subsize1 = String.length sub1 in 
if (i + subsize1) < size then check s (i + subsize1) ((Tok_ID(sub1))::tok_lst) else ((Tok_ID(sub1))::tok_lst)
  
(*get token for negative number *)
else if (Str.string_match (Str.regexp "(-[0-9]+)") s i) = true then 
let sub1 = string_after (matched_string s) 1 in 
let sub2 = string_before sub1 ((String.length sub1) - 1) in 
let subsize1 = (String.length sub1) + 2 in 
if (i + subsize1) < size then check s (i + (subsize1 - 1))((Tok_Int(int_of_string sub2))::tok_lst) else ((Tok_Int(int_of_string sub2))::tok_lst)

(*get token for number*)
else if (Str.string_match (Str.regexp "[0-9]+") s i) = true then 
let sub1 = matched_string s in 
let subsize1 = String.length sub1 in 
if (i + subsize1) < size then check s (i + subsize1) ((Tok_Int(int_of_string sub1))::tok_lst) else ((Tok_Int(int_of_string sub1))::tok_lst)

(*get token for l paren*)
else if (Str.string_match (Str.regexp"(") s i) = true then
if (i + 1) < size then check s (i + 1) ((Tok_LParen)::tok_lst) else ((Tok_LParen)::tok_lst)

(* get token for r paren*)
else if (Str.string_match (Str.regexp")") s i) = true then 
if (i + 1) < size then check s (i + 1) ((Tok_RParen)::tok_lst) else ((Tok_RParen)::tok_lst)

(* keep processing*)
else if i < size then check s (i + 1) tok_lst else tok_lst


let tokenize input = 
let tokl = check input 0 [] in 
rev tokl
