#lang racket
(provide (rename-out [replace:asm-interp    asm-interp]
                     [replace:asm-interp/io asm-interp/io])
         (except-out (all-from-out a86/interp)
                     asm-interp
                     asm-interp/io))
(require a86/interp)

(define (replace:asm-interp instructions)
  (match (asm-interp instructions)
    ['err              'err]
    [(cons _ bits)     bits]
    [(? integer? bits) bits]))

(define (replace:asm-interp/io instructions input)
  (match (asm-interp/io instructions input)
    [(cons 'err output)              (cons 'err output)]
    [(cons (cons _ bits) output)     (cons bits output)]
    [(cons (? integer? bits) output) (cons bits output)]))
