#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

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
    [(Lit l)        (compile-value l)]
    [(Prim0 p)      (compile-prim0 p)]
    [(Prim1 p e)    (compile-prim1 p e)]
    [(If e1 e2 e3)  (compile-if e1 e2 e3)]
    [(Begin e1 e2)  (compile-begin e1 e2)]
    [(Begin0 e1 e2) (compile-begin0 e1 e2)])) ;; NEW

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op0 -> Asm
(define (compile-prim0 p)
  (compile-op0 p))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr -> Asm
(define (compile-begin0 e1 e2)
  ;; TODO
  (seq  (compile-e e1)            ;;compile e1 and get val in rax
        (Push 'rax)               ;;push val from rax onto stack for later
        (compile-e e2)            ;;compile e2 and now rax is clobbered
        (Pop 'rax)                ;;pop saved rax value from stack back into rax to return e1
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
