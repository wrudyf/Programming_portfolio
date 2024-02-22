#lang racket
;; NOTE: New file.
(provide test/set-box! test/io/set-box!)
(require rackunit)

(define (test/set-box! run)
  (begin ;; Testing set-box!
    (check-equal? (run '(let ([b (box 1)])
                          (set-box! b 2)))
                  (void))
    (check-equal? (run '(let ([b (box 1)])
                          (begin (set-box! b 2)
                                 (unbox b))))
                  2)
    ;; TODO: Add non-I/O tests here, if you like.
    (check-equal? (run 15)
                  15)

    ;;i think this is the only test i need
    (check-equal? (run '(unbox (set-box! (box 5) 15) ))
                  15)

    ))

(define (test/io/set-box! run)
  (begin ;; Testing set-box!
    (check-equal? (run "a" '(let ([b (box 1)])
                              (begin (set-box! b (read-byte))
                                     (unbox b))))
                  (cons 97 ""))
    ;; TODO: Add I/O tests here, if you like.
    ))
