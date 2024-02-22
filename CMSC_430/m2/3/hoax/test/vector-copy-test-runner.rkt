#lang racket
;; NOTE: New file.
(provide test/vector-copy* test/io/vector-copy*)
(require rackunit)

(define (test/vector-copy* run)
  (begin ;; Testing vector-copy*
    (check-equal? (run '(let ([v (make-vector 3 #t)])
                          (vector-copy* v 3)))
                  #(#t #t #t))
    (check-equal? (run '(let ([v (make-vector 3 #t)])
                          (vector-copy* v 2)))
                  #(#t #t))
    (check-equal? (run '(let ([v (make-vector 3 #t)])
                          (begin (vector-set! v 1 #f)
                                 (vector-copy* v 2))))
                  #(#t #f))
    (check-equal? (run '(let ([v1 (make-vector 3 #t)])
                          (let ([v2 (vector-copy* v1 2)])
                            (begin (vector-set! v2 1 #f)
                                   v1))))
                  #(#t #t #t))
    (check-equal? (run '(let ([v1 (make-vector 3 #t)])
                          (let ([v2 (vector-copy* v1 2)])
                            (begin (vector-set! v2 1 #f)
                                   v2))))
                  #(#t #f))
    ;; TODO: Add non-I/O tests here, if you like.
    ;;i think this is the only test i need, it is right
    (check-equal? (run '(let ([v (make-vector 3 #t)])
                          (vector-copy* v 4)))
                  'err)
    ))

(define (test/io/vector-copy* run)
  (begin ;; Testing vector-copy*
    (check-equal? (run "a" '(let ([v (make-vector 3 (read-byte))])
                              (vector-copy* v 3)))
                  (cons #(97 97 97) ""))
    ;; TODO: Add I/O tests here, if you like.
    ))
