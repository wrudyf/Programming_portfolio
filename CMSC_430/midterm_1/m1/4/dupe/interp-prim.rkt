#lang racket
(provide interp-prim1)

;; Op Value -> Value
(define (interp-prim1 op v)
  (match op
    ['add1  (add1 v)]
    ['sub1  (sub1 v)]
    ['zero? (zero? v)]
    ['odd? (odd? v)]
    ))
