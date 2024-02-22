#lang racket
;;gonna test apply with nested lambdas
(define g
(case-lambda
[(x) (+ x x)]
[(y) (- y y)]
)
)

(define f
(case-lambda
[(x y) #f]
[(x) (g 5 10)]
))

(apply f 1 (add1 5) )
