open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks = 
let t, exp = parseExpr toks in 
t, exp

(*parse the expr production rule*)
and parseExpr toks = 

match (lookahead toks) with
| Some Tok_Let -> parseLetExpr toks

| Some Tok_In -> parseExpr (match_token toks Tok_In)
| Some Tok_If -> parseIfExpr toks
| Some Tok_Then -> parseExpr (match_token toks Tok_Then)
| Some Tok_Else -> parseExpr (match_token toks Tok_Else)
| Some Tok_Fun -> parseFunctionExpr toks
| Some Tok_Arrow -> parseExpr (match_token toks Tok_Arrow)
| _ -> parseOrExpr toks

(*parse LetExpr*)
and parseLetExpr toks = 
(*pop off let token*)
let l_one = match_token toks Tok_Let in

(*see if next token is rec or not*)
let c = match lookahead l_one with 
Some Tok_Rec -> true
| _ -> false in 
if c = false then 
(*get some ID token*)
let t_ID = lookahead l_one in 
(*get ID token string*)
let t_tok = match t_ID with 
Some t -> t 
|_ -> failwith "error" in 
(*pop off id token*)
let l_two = match_token l_one t_tok in
(*pop off equal token*)
let l_three = match_token l_two Tok_Equal in
(*pattern match and fill in let expression*)
let t_ID_ex = match t_ID with 
Some h -> h
| _ -> failwith "error" in   
(*get token to pass into Let exp*) 
let t_s = match t_ID_ex with 
Tok_ID s -> s 
| _ -> failwith "error" in   
(*get first expression *)
let lt, e1 = parseExpr l_three in 
(*get second expression *)
let rt, e2 = parseExpr lt in 
(rt, Let (t_s, false, e1, e2))

else 
(*pop off Rec token*)
let l_no_rec = match_token l_one Tok_Rec in  
let t_ID = lookahead l_no_rec in 
(*get ID token string*)
let t_tok = match t_ID with 
Some t -> t 
|_ -> failwith "error" in 
(*pop off id token*)
let l_two = match_token l_no_rec t_tok in
(*pop off equal token*)
let l_three = match_token l_two Tok_Equal in
(*pattern match and fill in let expression*)
let t_ID_ex = match t_ID with 
Some h -> h
| _ -> failwith "error" in   
(*get token to pass into Let exp*) 
let t_s = match t_ID_ex with 
Tok_ID s -> s 
| _ -> failwith "error" in   

(*get first expression *)
let lt, e1 = parseExpr l_three in 
(*get second expression *)
let rt, e2 = parseExpr lt in 
(rt, Let (t_s, true, e1, e2))


(*parse IfExpr*) 
and parseIfExpr toks = 
(*pop off if token*)
let l_one = match_token toks Tok_If in 
(*parse to get expr1 *)
let t1, e1 = parseExpr l_one in 
(*parse to get expr2 *)
let t2, e2 = parseExpr t1 in 
(*parse to get expr3 *)
let t3, e3 = parseExpr t2 in 
(t3, If(e1, e2, e3)) 

(*parse FunctionExpr*)
and parseFunctionExpr toks = 
(*pop off fun token*) 
let l_one = match_token toks Tok_Fun in 
(*get some ID token*)
let t_ID = lookahead l_one in 
(*get ID token*)
let t_tok = match t_ID with 
Some t -> t 
| _ -> failwith "error" in
(*pop off id token*)
let l_two = match_token l_one t_tok in
let t_ID_ex = match t_ID with 
Some h -> h 
|_ -> failwith "error" in 
(*get token string to pass into Let exp*) 
let t_s = match t_ID_ex with 
Tok_ID s -> s 
| _ -> failwith "error" in
(*pop off arrow token*)
let l_three = match_token l_two Tok_Arrow in 
let t1, e1 = parseExpr l_three in 
(t1, Fun(t_s, e1))

(*parse OrExpr*)
and parseOrExpr toks = 
let (t, e) = parseAndExpr toks in 
match lookahead t with 
| Some Tok_Or -> let t2 = match_token t Tok_Or in 
            let (t3, e2) = parseOrExpr t2 in 
            (t3, Binop(Or, e, e2))
| _ -> t, e

(*parse AndExpr*)
and parseAndExpr toks = 
let (t, e) = parseEqualityExpr toks in 
match lookahead t with
| Some Tok_And -> let t2 = match_token t Tok_And in 
            let (t3, e2) = parseAndExpr t2 in 
            (t3, Binop(And, e, e2))
| _ -> t, e

(*parse EqualityExpr*)
and parseEqualityExpr toks = 
let (t, e) = parseRelationalExpr toks in 
match lookahead t with 
| Some Tok_Equal -> let t2 = match_token t Tok_Equal in 
                let (t3, e2) = parseEqualityExpr t2 in 
                (t3, Binop(Equal, e, e2))
| Some Tok_NotEqual -> let t2 = match_token t Tok_NotEqual in 
                let (t3, e2) = parseEqualityExpr t2 in 
                (t3, Binop(NotEqual, e, e2))
| _ -> t, e

(*parse RelationalExpr*)
and parseRelationalExpr toks = 
let (t, e) = parseAdditiveExpr toks in 
match lookahead t with 
| Some Tok_Less -> let t2 = match_token t Tok_Less in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(Less, e, e2))
| Some Tok_Greater -> let t2 = match_token t Tok_Greater in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(Greater, e, e2))
| Some Tok_GreaterEqual -> let t2 = match_token t Tok_GreaterEqual in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(GreaterEqual, e, e2))
| Some Tok_LessEqual -> let t2 = match_token t Tok_LessEqual in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(LessEqual, e, e2))
| _ -> t, e

(*parse AdditiveExpr*)
and parseAdditiveExpr toks = 
let (t, e) = parseMultiplicativeExpr toks in 
match lookahead t with 
| Some Tok_Add -> let t2 = match_token t Tok_Add in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(Add, e, e2))
| Some Tok_Sub -> let t2 = match_token t Tok_Sub in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(Sub, e, e2))
| _ -> t, e

(*parse MultiplicativeExpr*) 
and parseMultiplicativeExpr toks = 
let (t, e) = parseConcatExpr toks in 
match lookahead t with 
| Some Tok_Mult -> let t2 = match_token t Tok_Mult in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(Mult, e, e2))
| Some Tok_Div -> let t2 = match_token t Tok_Div in 
                let (t3, e2) = parseRelationalExpr t2 in 
                (t3, Binop(Div, e, e2))
| _ -> t, e

(*parse ConcatExpr*)
and parseConcatExpr toks = 
let (t, e) = parseUnaryExpr toks in 
match lookahead t with 
| Some Tok_Concat -> let t2 = match_token t Tok_Concat in 
                let (t3, e2) = parseConcatExpr t2 in 
                (t3, Binop(Concat, e, e2))
| _ -> t, e

(*parse UnaryExpr*)
and parseUnaryExpr toks = 
match lookahead toks with 
| Some Tok_Not -> let t = match_token toks Tok_Not in 
                let t2, e = parseUnaryExpr t in
                (t2, Not(e))
| _ -> parseFunctionCallExpr toks

(*parse FunctionCallExpr*)
and parseFunctionCallExpr toks = 
let (t, e) = parsePrimaryExpr toks in 
match lookahead t with 
| Some Tok_Int i -> let t2, e2 = parsePrimaryExpr t in 
                    (t2, FunctionCall(e, e2))
| Some Tok_Bool b -> let t2, e2 = parsePrimaryExpr t in 
                    (t2, FunctionCall(e, e2))
| Some Tok_String s -> let t2, e2 = parsePrimaryExpr t in 
                    (t2, FunctionCall(e, e2))
| Some Tok_ID x -> let t2, e2 = parsePrimaryExpr t in 
                    (t2, FunctionCall(e, e2))
| _ -> t,e

(*parse PrimaryExpr *)
(*this is right, just add case for parenthesis*)
and parsePrimaryExpr toks = 
match (lookahead toks) with 
| Some Tok_Int i -> let t = match_token toks (Tok_Int i) in 
                (t, Value(Int (i)))
| Some Tok_Bool b -> let t = match_token toks (Tok_Bool b) in 
                (t, Value(Bool (b)))
| Some Tok_String s -> let t = match_token toks (Tok_String s) in 
                (t, Value(String(s)))
| Some Tok_ID x -> let t = match_token toks (Tok_ID x) in 
                (t, ID(x))
| Some Tok_LParen -> let t = match_token toks Tok_LParen in 
                let (t2, e1) = parseExpr t in 
                let t3 = match_token t2 Tok_RParen in 
                (t3, e1)
| _ -> raise (InvalidInputException("failure at parse primary"))

(* Part 3: Parsing mutop *)
let rec rev lst = match lst with
[] -> []
| h::t -> (rev t)@[h]

let rec sizetoks toks x = match toks with
[] -> x
| h::t -> sizetoks t (x + 1)

let rec fold f a l = match l with 
[] -> a
| h::t -> fold f (f a h) t

let check toks = 
let size1 = sizetoks toks 0 in 
let q = fold (fun x y -> 
match x with 
a, b -> if (a = (size1 - 1)) then 
if y = Tok_DoubleSemi then
(a + 1, b)
else 
raise (InvalidInputException("didn't end with double semi"))
else (a + 1, y::b)
) (0, []) toks
in match q with 
a, b -> (rev b)

let rec parse_mutop toks = 
match lookahead toks with 
|Some Tok_DoubleSemi -> ([], NoOp)
| _ ->
let ntoks = check toks in
let (t, exp) = parseMutop ntoks in 
(t, exp)

and parseMutop toks = 

match lookahead toks with 
| Some Tok_Def -> parseDefMutop toks
| Some Tok_DoubleSemi -> ([], NoOp)
| _ -> parseExprMutop toks

and parseDefMutop toks = 
(*pop off def token*)
let t1 = match_token toks Tok_Def in
(*get some token ID*)
let tok_ID = match lookahead t1 with 
Some Tok_ID x -> x 
| _ -> failwith "error" in 
(*pop off ID token*)
let t2 = match_token t1 (Tok_ID (tok_ID)) in 
(*pop off equal token*)
let t3 = match_token t2 Tok_Equal in 

let (t4, e1) = parse_expr t3 in 
(t4, Def(tok_ID, e1))

and parseExprMutop toks = 
let (t, e) = parse_expr toks
in (t,Expr(e))
