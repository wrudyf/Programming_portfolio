#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" "interp.rkt" a86/ast)

;; Registers used
(define rax 'rax)
(define rsp 'rsp)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Global 'entry)
        (Label 'entry)
        (Sub rsp 8)
        (compile-e e)
        (Add rsp 8)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit l)       (compile-value l)]
    [(Prim0 p)     (compile-prim0 p)]
    [(Prim1 p e)   (compile-prim1 p e)]
    [(If e1 e2 e3) (compile-if e1 e2 e3)]
    [(Begin e1 e2) (compile-begin e1 e2)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op0 -> Asm
(define (compile-prim0 p)
  (compile-op0 p))

;; Op1 Expr -> Asm
;;function that takes in ast to see if we have a read or peek byte
(define (no_read t)
  (match t
  [(Lit l) #t]
  [(Prim0 p) #f]
  [(Prim1 p e) (no_read e)]
  [(If e1 e2 e3) (and (no_read e1) (no_read e2) (no_read e3))]
  [(Begin e1 e2) (and (no_read e1) (no_read e2))]
  )
)
(define (compile-prim1 p e)
  (if (no_read e)               ;;if we have no read or peek byte, then we'll interp

  (let ((v (interp e) ))
       (seq (Mov 'rax (value->bits v))
            (compile-op1 p)))

  (seq  (compile-e e)           ;;else, we'll just compile-e and compile op1 like normally
        (compile-op1 p)
  )
  )
)


;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

;; Expr Expr -> Asm
(define (compile-begin e1 e2)
  (seq (compile-e e1)
       (compile-e e2)))
