#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; Expr -> Value
(define (interp e)
  (match e
    [(AndE es) (interp-and es)] ;; NEW
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e0)
     (interp-prim1 p (interp e0))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    [(Begin e1 e2)
     (begin (interp e1)
            (interp e2))]))

;; NEW
;; [Listof Expr] -> Value
(define (interp-and es)
  (match es
    ['() #t]
    [(cons e '())
     (interp e)]
    [(cons e es)
     (and (interp e)
          (interp-and es))]))
