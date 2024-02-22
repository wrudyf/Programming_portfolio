#lang racket

;;testing apply here with rest
(define (f x . xs)
    xs
)

(apply f 1 (cons 1 '()))
