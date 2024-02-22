#lang racket

;;idea for test, testing case lambda for same conditions and see if it crashes anything

#|
(define f
    (case-lambda
        [x #f]
        [y (let (x 1) (add1 (+ y 0 x 1)) ) ]))

(f 1)|#

(define f
    (case-lambda
        [x #f]
        [y (add1 5)]
))

(f 1)
