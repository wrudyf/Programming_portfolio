#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(require "vector-copy-test-runner.rkt")

(test (λ (e) (interp (parse e))))
(test/io (λ (in e) (interp/io (parse e) in)))

;; NOTE: New.
(test/vector-copy* (λ (e) (interp (parse e))))
;; NOTE: New.
(test/io/vector-copy* (λ (in e) (interp/io (parse e) in)))
