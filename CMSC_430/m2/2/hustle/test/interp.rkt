#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(require "set-box-test-runner.rkt")  ;; NOTE: New.

(test (λ (e) (interp (parse e))))
(test/io (λ (in e) (interp/io (parse e) in)))

;; NOTE: New.
(test/set-box! (λ (e) (interp (parse e))))
;; NOTE: New.
(test/io/set-box! (λ (in e) (interp/io (parse e) in)))
