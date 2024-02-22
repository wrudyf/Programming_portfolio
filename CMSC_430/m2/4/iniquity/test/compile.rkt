#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")
(require "default-define-test-runner.rkt")  ;; NOTE: New.

(test (λ p (run (compile (apply parse p)))))
(test/io (λ (in . p) (run/io (compile (apply parse p)) in)))

;; NOTE: New.
(test/default-define (λ p (run (compile (apply parse p)))))
;; NOTE: New.
(test/io/default-define (λ (in . p) (run/io (compile (apply parse p)) in)))
