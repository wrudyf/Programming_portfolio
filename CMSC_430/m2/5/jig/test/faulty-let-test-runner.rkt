#lang racket
;; NOTE: New file.
(provide test/faulty-let test/io/faulty-let)
(require rackunit)

(define (test/faulty-let run)
  (begin ;; Testing the faulty let.
    (check-equal? (run '(add1 0))
                  1)
    (check-equal? (run '(let ((x 5)) x) )
                  5)

    (check-equal? (run
                  '(define (f y z)
                  (if (zero? y) z (f (- y 1) (+ 2 z)))
                  )
                  '(let ((x (f 3 3))
                              )

                (f x 0))
                )
                  5)

                  ;;i don't know what test to write, sorry, but i did explain my answer before
    (check-equal? (run '(let ((x 87))
              (let ((x 13)) x)
              ) )
                  13)

                  ))

(define (test/io/faulty-let run)
  (begin ;; Testing the faulty let.
    (check-equal? (run "a"
                       '(read-byte))
                  (cons 97 ""))))
