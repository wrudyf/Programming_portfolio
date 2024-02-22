#lang racket
;;does this break anything?
;;idk why this breaks some compilers, it should work
(let ((x 1)) (add1 x) )

;;i know this should break some compilers because our fraud only handles nums up to 2^32 if signed...and up to 2^62 if unsigned
(add1 36893488147419103232)

;;does this break anything?
;;no
(let ((x 1)) (begin (add1 x) (add1 x)))

(let ((x 1)) (begin (add1 x) (let* ((x x)) (* x 5000000000000000000000000000000000000000000000000000))))

