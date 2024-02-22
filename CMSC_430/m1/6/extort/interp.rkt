#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; Expr -> Answer
(define (interp e)
  (match e
    [(Lit l)  l]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e0)
     (match (interp e0)       
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(If e1 e2 e3)
     (match (interp e1)
       ['err 'err]
       [v
        (if v
            (interp e2)
            (interp e3))])]
    [(Begin e1 e2)
     (match (interp e1)
       ['err 'err]
       [_ (interp e2)])]
    [(Repeat e1 e2)
     (match (interp e1)
       [1 (interp e2)]
       [(? positive-integer? i)
        (interp-repeat (sub1 i) e2)]
       [_ 'err])]))


(define (interp-repeat i e2)
  (match (interp e2)
    ['err 'err]
    [v (if (zero? i)
           v
           (interp-repeat (sub1 i) e2))]))

