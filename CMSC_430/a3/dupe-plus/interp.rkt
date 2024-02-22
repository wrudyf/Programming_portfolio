#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (eval-icon x)
(match x
[(Clause a b) (if (interp a) (interp b) #f)]
[ _ error "error evaluating condition"]
))

(define (interp-cond x)
(match x
['() #f]
[_ (if (eval-icon (first x)) (eval-icon (first x)) (interp-cond (rest x))) ]))

(define (contains lst x)
(match lst
[ '() #f]
[ _ (if (equal? (first lst) x) #t (contains (rest lst) x)) ]))

;;evaluate individual clause to get value
(define (eval-icase c)
(match c
[ (Clause lst b) (interp b) ]
))

(define (gett x)
(match x
[(Clause a b) (first  (rest a))]
))

(define (interp-case c x)
(match x
['() #f]
;;evaluate each individual clause by getting first from list
[(cons a b)
(if (match a [(Clause d e) (if (contains d c) #t #f)])
(match a [(Clause f g) (interp g)])
(interp-case c b) )]
))

(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]

    ;; TODO: Handle cond
    [(Cond cs e)
      (if (interp-cond cs) (interp-cond cs) (interp e))
    ]

    ;; TODO: Handle case
    [(Case e cs el)
    (if  (interp-case (interp e) cs) (interp-case (interp e) cs) (interp el))
    ]

    ))



