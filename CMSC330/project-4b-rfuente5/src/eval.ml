open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*
type values = Int of int|Bool of bool|String of string
*)
(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
match e with 
| Value(Int i) -> Int i
| Value(Bool b) -> Bool b
| Value(String s) -> String s
| ID(x1) -> (ref_lookup env x1)

(*evaluate add, sub, mult, or div*)
| Binop (op, e1, e2) -> 
        (match op with 
        | Add -> let v1 = (eval_expr env e1) in
                 let v2 = (eval_expr env e2) in 
                (match v1, v2 with 
                Int a, Int b -> Int (a + b)
                | _ -> raise (TypeError("invalid input on add"))
                )
        
        | Sub -> let v1 = (eval_expr env e1) in 
                 let v2 = (eval_expr env e2) in 
                (match v1, v2 with 
                Int a, Int b -> Int (a - b)
                | _ -> raise (TypeError("invalid input on sub"))
                )

        | Mult -> let v1 = (eval_expr env e1) in 
                  let v2 = (eval_expr env e2) in 
                (match v1, v2 with 
                Int a, Int b -> Int (a * b)
                | _ -> raise (TypeError("invalid input on mult"))
                )
        | Div -> let v1 = (eval_expr env e1) in 
                 let v2 = (eval_expr env e2) in 
                (match v1, v2 with 
                Int a, Int b -> if b = 0 then raise(DivByZeroError) else
                                Int (a / b)
                | _ -> raise (TypeError("invalid input"))
                )
        
        | Greater -> let v1 = (eval_expr env e1) in 
                     let v2 = (eval_expr env e2) in 
                     (match v1, v2 with 
                     Int a, Int b -> let b = (a > b) in Bool b
                     | _ -> raise (TypeError("invalid input"))   
                     )
        | GreaterEqual -> let v1 = (eval_expr env e1) in 
                          let v2 = (eval_expr env e2) in 
                          (match v1, v2 with 
                          Int a, Int b -> let b = (a >= b) in Bool b
                          | _ -> raise (TypeError("invalid input"))   
                          )
                
        | Less ->    let v1 = (eval_expr env e1) in 
                     let v2 = (eval_expr env e2) in 
                     (match v1, v2 with 
                     Int a, Int b -> let b = (a < b) in Bool b
                     | _ -> raise (TypeError("invalid input"))   
                     )
        | LessEqual -> let v1 = (eval_expr env e1) in 
                       let v2 = (eval_expr env e2) in 
                       (match v1, v2 with 
                       Int a, Int b -> let b = (a <= b) in Bool b
                       | _ -> raise (TypeError("invalid input"))   
                       )
        | Concat ->  let v1 = (eval_expr env e1) in 
                     let v2 = (eval_expr env e2) in 
                     (match v1, v2 with 
                     String a, String b -> String (a^b)
                     | _ -> raise (TypeError("invalid input"))   
                     )
        
        | Equal ->   let v1 = (eval_expr env e1) in 
                     let v2 = (eval_expr env e2) in 
                     (match v1, v2 with 
                     | Int a, Int b -> let b = (a = b) in Bool b
                     | Bool a, Bool b -> let c = (a = b) in Bool c
                     | String a, String b -> let c = (a = b) in Bool c
                     | _ -> raise (TypeError("invalid input"))   
                     )
                
        | NotEqual ->let v1 = (eval_expr env e1) in 
                     let v2 = (eval_expr env e2) in 
                     (match v1, v2 with 
                     | Int a, Int b -> let b = (a <> b) in Bool b
                     | Bool a, Bool b -> let c = (a <> b) in Bool c
                     | String a, String b -> let c = (a <> b) in Bool c
                     | _ -> raise (TypeError("invalid input"))   
                     )
                
        | Or -> let v1 = (eval_expr env e1) in 
                let v2 = (eval_expr env e2) in 
                (match v1, v2 with 
                Bool a, Bool b -> Bool (a || b)
                | _ -> raise (TypeError("invalid input"))   
                )
                
        | And ->let v1 = (eval_expr env e1) in 
                let v2 = (eval_expr env e2) in 
                (match v1, v2 with 
                Bool a, Bool b -> Bool (a && b)
                | _ -> raise (TypeError("invalid input"))   
                )
        
        )
| If (e1, e2, e3) -> 
        let b = eval_expr env e1 in 
        let v1 = eval_expr env e2 in 
        let v2 = eval_expr env e3 in 
        (match b, v1, v2 with 
        Bool b,  c,  d-> if b then c else d
        | _ -> raise (TypeError("invalid input"))
        )        

| Let (x, r, e1, e2) ->  
        (*non recursive *)   
        let v1 = eval_expr env e1 in 
        let u_env = ref_extend env x v1 in 
        eval_expr u_env e2
        (*do non recursive*)

| Fun (x, e1) -> 
    
    Closure(env, x, e1)

| FunctionCall(e1, e2) -> 
    (*this should return a closure*)    
    let env1 = eval_expr env e1 in  
    (*evaluate e2 to value*)    
    (*let v1 = eval_expr env e2 in *)
    (*evaluate e1 in A*)
    (match env1 with 
    Closure(en, f, e) -> 
    let f1 = ref_lookup en f in 
    f1
    |_ -> raise (TypeError("invalid input"))
    )
| _ -> raise (TypeError("invalid input"))


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
match m with 
| NoOp -> (env, None)
| _ -> raise (TypeError("invalid input"))
