#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")
(require "set-box-test-runner.rkt")  ;; NOTE: New.

(test (λ (e) (run (compile (parse e)))))
(test/io (λ (in e) (run/io (compile (parse e)) in)))

;; NOTE: New.
(test/set-box! (λ (e) (run (compile (parse e)))))
;; NOTE: New.
(test/io/set-box! (λ (in e) (run/io (compile (parse e)) in)))
