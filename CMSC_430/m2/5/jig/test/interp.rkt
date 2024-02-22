#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(require "faulty-let-test-runner.rkt")  ;; NOTE: New.

(test (λ p (interp (apply parse p))))
(test/io (λ (in . p) (interp/io (apply parse p) in)))

;; NOTE: New.
(test/faulty-let (λ p (interp (apply parse p))))
;; NOTE: New.
(test/io/faulty-let (λ (in . p) (interp/io (apply parse p) in)))
