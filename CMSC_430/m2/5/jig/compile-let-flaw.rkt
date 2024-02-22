#lang racket
;; NOTE: New file.
;; TODO: Replace the program below with one that demonstrates the issue.
;;so, one issue i see is with
;;(seq (compile-e e1 c #f) and (seq (compile-e e1 c t?)
;;in one, we are passing #f, and in the other, we are passing the variable t?
;;in a let expression, there is e1 and then e2. e1 is never in tail position, but e2 is in tail
;;position, so when we say (compile-e e1 c t?) we give e1 the possibility of being in tail
;;position when it's not, and that's wrong because e1 is not the last thing we have to do, e2 is


(let ((x 87))
(let ((x 13)) x)
)
