#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer


(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive
    [(PrimN p es)
      (match (interp*-env es r)
      ;;error return error
      ['err 'err]
      ;;or it will be a list of values
      [vlst (interp-primN p vlst)]
      )
    ]

    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e)
    ;;still think about how to check for errors in cond *****************************
    (if (interp-cond cs r) (interp-cond cs r) (interp-env e r))

    ]
    ;; TODO: implement case
    [(Case ev cs el)
    ;;still think about how to check for errors in case *****************************
    (match (interp-env ev r)
    ['err 'err]
    [ _ (if (interp-case (interp-env ev r) cs r) (interp-case (interp-env ev r) cs r) (interp-env el r))]
    )
    ]


    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    ;;get list of id's get list of e's with id's, and final e to evaluate
    [
    (Let idlst exlst e)
     (match (interp*-env exlst r)
        ['err 'err]
        ['() (interp-env e r)]
        [vlst  (interp-env e (zip r idlst vlst) )])
    ]

;;start HERE **************************************************************************
;;HAVE TO DEBUG LET*...ISSUE WITH LET* INSIDE LET...DEBUG
    ;; TODO: implement let*
    [(Let* idlst exlst e) (match (interp*-env exlst r)
        ['err 'err]
        ['() (interp-env e r)]
        [vlst (interp-env e  (dynamic_zip r idlst vlst) )]
    )]
    ))

;;helper function for interpreting let* *************************************************
;;idea: same kind of idea as zip except...maybe we will have to look up some stuff
(define (dynamic_zip r idlst exlst)
(match exlst
['() r]
[lst (match (first lst)
;;if var x, look up to get val
[(Var x)  (dynamic_zip (cons (list (first idlst) (lookup r x)) r) (rest idlst) (rest lst))]
;;otherwise, just zip normally
[_ (dynamic_zip (cons (list (first idlst) (first exlst)) r) (rest idlst) (rest lst))]
)]
))

;;helper functions for interpreting let *************************************************
;;idea: put all of the variables in environment first and then evaluate the expression
(define (zip r lst1 lst2)
(match lst1
['() r]
[_ (zip (cons (list (first lst1) (first lst2)) r) (rest lst1) (rest lst2))]
))

(define (exists_in_env r x)
(match r
['() #f]
[(cons (list y val) r)
  (if (symbol=? x y)
      #t
      (lookup r x)
  )
]
)
)

;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*

(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))


;;what happens if we don't have value? can we just assume lookup won't have an error?
;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))


;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

;;helper functions for interpreting cond ************************************************
;;note, check for assertions???
(define (interp-cond x env)
(match x
['() #f]
[_ (if (eval-icon (first x) env) (eval-icon (first x) env) (interp-cond (rest x) env)) ]))

(define (eval-icon x env2)
(match x
[(Clause a b) (if (interp-env a env2) (interp-env b env2) #f)]
[ _ error "error evaluating condition"]
))


;;helper functions for interpreting case ************************************************
;;note, check for assertions???
(define (contains lst x)
(match lst
[ '() #f]
[ _ (if (equal? (first lst) x) #t (contains (rest lst) x)) ]))

(define (interp-case c x env)
(match x
['() #f]
;;evaluate each individual clause by getting first from list
[(cons a b)
(if (match a [(Clause d e) (if (contains d c) #t #f)])
(match a [(Clause f g) (interp-env g env)])
(interp-case c b env) )]
))
