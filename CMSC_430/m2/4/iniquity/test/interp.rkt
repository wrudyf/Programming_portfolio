#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(require "default-define-test-runner.rkt")  ;; NOTE: New.

(test (λ p (interp (apply parse p))))
(test/io (λ (in . p) (interp/io (apply parse p) in)))

;; NOTE: New.
(test/default-define (λ p (interp (apply parse p))))
(test/io/default-define (λ (in . p) (interp/io (apply parse p) in)))
