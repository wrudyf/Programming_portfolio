#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(require "new-patterns-test-runner.rkt")  ;; NOTE: New.

(test (λ p (interp (apply parse p))))
(test/io (λ (in . p) (interp/io (apply parse p) in)))

;; NOTE: New.
(test/new-patterns (λ p (interp (apply parse p))))
;; NOTE: New.
(test/io/new-patterns (λ (in . p) (interp/io (apply parse p) in)))
