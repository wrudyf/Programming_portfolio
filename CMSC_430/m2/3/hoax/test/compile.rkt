#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")
(require "vector-copy-test-runner.rkt")  ;; NOTE: New.

(test (λ (e) (run (compile (parse e)))))
(test/io (λ (in e) (run/io (compile (parse e)) in)))

;; NOTE: New.
(test/vector-copy* (λ (e) (run (compile (parse e)))))
;; NOTE: New.
(test/io/vector-copy* (λ (in e) (run/io (compile (parse e)) in)))
