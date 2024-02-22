#lang racket
;; NOTE: New file.
(provide test/default-define test/io/default-define)
(require rackunit)

(define (test/default-define run)
  (begin ;; Testing define with default parameter.
    (check-equal? (run '(define (foo x [y 0]) (+ x y))
                       '(foo 2 3))
                  5)
    ;; NOTE: This test fails with the starter code.

    ;;i think this test is good enough here
    (check-equal? (run '(define (foo x [y 0]) (+ x y))
                       '(foo 2))
                  2)
    ;; TODO: Add non-I/O tests here, if you like.
    ))

(define (test/io/default-define run)
  (begin ;; Testing define with default parameter.
    (check-equal? (run "a"
                       '(define (bar x y [z (read-byte)])
                          (if (zero? x)
                              y
                              (write-byte (+ x z))))
                       '(bar 0 1 2))
                  (cons 1 ""))
    ;; NOTE: This test fails with the starter code.
    (check-equal? (run "a"
                       '(define (bar x y [z (read-byte)])
                          (if (zero? x)
                              y
                              (write-byte (+ x z))))
                       '(bar 1 2))
                  (cons (void) "b"))
    ;; TODO: Add I/O tests here, if you like.
    ))
