#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")
(require "faulty-let-test-runner.rkt")  ;; NOTE: New.

(test (λ p (run (compile (apply parse p)))))
(test/io (λ (in . p) (run/io (compile (apply parse p)) in)))

;; NOTE: New.
(test/faulty-let (λ p (run (compile (apply parse p)))))
;; NOTE: New.
(test/io/faulty-let (λ (in . p) (run/io (compile (apply parse p)) in)))
