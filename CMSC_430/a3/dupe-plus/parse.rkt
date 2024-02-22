#lang racket
(provide parse)
(require "ast.rkt")

(define (get_last x y)
(match x
['() y]
[ _ (get_last (rest x) (first x))]))

(define (pop_last x)
(match x
['() '()]
[ _ (reverse (rest (reverse x)) ) ]
))

;; modify get icond to take into account if a in (list a b) is a list itself
(define (get_icond x)
  (match x

  [ (list a b) (Clause (parse a)  (parse b))]
  [ (list (cons a b) c) (Clause (cons a b) (parse c))]

  ['() '()]
  [ _ error "erorr getting condition for cond"]))

(define (mconds x)
(match x
['() '()]
[ _ (cons (get_icond (first x)) (mconds (rest x))) ]))


(define (get_i2conds x)
(match x
[ (list a b) (Clause a (parse b))]
[ '() '()]
[ _ error "error getting condition for case"]))

(define (m2conds x)
(match x
['() '()]
[(cons a '()) (cons (get_i2conds (first a)) '())]
[ _ (cons (get_i2conds (first x)) (m2conds (rest x))) ]
))

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1 (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    ;; TODO: Handle abs, - and not
    [(list 'abs e) (Prim1 'abs (parse e)) ]
    [(list '- e) (Prim1 '- (parse e))]
    [(list 'not e) (Prim1 'not (parse e))]


    [(list 'else e) (parse e)]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]

    ;; TODO: Handle cond
    ;;match as a cons

    [(list 'else e) (parse e)]
    [(cons 'cond b)
    (Cond (mconds (pop_last b)) (parse (get_last b '()) ) ) ]

    ;; TODO: Handle case
    ;;for case still have to take care of middle struct to pattern match on nested lists
    [(cons 'case b)

    ;;go over (rest (pop_last b))) because this makes an error with empty lists

    ;;(Case (parse (first b)) (m2conds (rest (pop_last b))) (parse (get_last b '())) )]
    (Case (parse (first b)) (parse_case_mid (pop_last (rest b)) ) (parse (get_last b '())) )]
    [_ "error parsing case"]
    ;;[ (list a) (a)]
    ))

(define (parse_case_elem x)
(match x
['() '()]
[(list a b) (Clause a (parse b))]
))

(define (parse_case_mid x)
(match x
['() '()]
[ _ (cons (parse_case_elem (first x)) (parse_case_mid (rest x))) ]
[ _ error"error parsing case mid"]
))
