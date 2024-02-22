#lang racket
;;testing more iniquity+ basic stuff like nested lambdas
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

(f 2)
