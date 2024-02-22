#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (Sub 'rsp 8)
        (compile-e e)
        (Add 'rsp 8)
        (Ret)
        ;; Error handler
        (Label 'err)
        (Call 'raise_error)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Prim0 p)          (compile-prim0 p)]
    [(Prim1 p e)        (compile-prim1 p e)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3)]
    [(Begin e1 e2)      (compile-begin e1 e2)]))

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

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'iftr))
        (l2 (gensym 'iffa))
        (l3 (gensym 'ifen))
        )
    (seq      (compile-e e1)                  ;;compiled first expr, now res is in rax
              (Cmp rax (value->bits false))   ;;compare rax to false
              (Jne l1)                        ;;if not equal to false, then check for true
              (compile-e e3)                  ;;if it is false, then compile e3 false
              (Jmp l3)                        ;;jump to end
              (Label l1)
              (Cmp rax (value->bits true))    ;;compare rax to true value
              (Jne 'err)                      ;;if not equal to true, then jump to err label
              (compile-e e2)                  ;;if it is true, then compile e2 true
              (Label l3)
         ))
         )

;; Expr Expr -> Asm
(define (compile-begin e1 e2)
  (seq (compile-e e1)
       (compile-e e2)))
