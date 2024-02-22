#lang racket
;;i don't know why add1 1 crashes some compiler
(add1 1)
;;does this extra stuff break anything?
(+ 5 10 (- 11 10))

(if (read-byte) (begin (+ 5 10) (- 10000 10000)) (let ((x 1000000)) (+ x 5)) )

(case '() ['() 5] [else 15])

(begin (add1 #f) (add1 2))

;;does this break anything?
;;idk why this breaks some compilers, it should work
(let ((x 1)) (add1 x) )

;;i know this should break some compilers because our fraud only handles nums up to 2^32 if signed...and up to 2^62 if unsigned
(add1 36893488147419103232)


;;does this break anything?
;;no
(let ((x 1)) (begin (add1 x) (add1 x)))

(let ((x 1)) (begin (add1 x) (let* ((x x)) (* x 5000000000000000000000000000000000000000000000000000))))



