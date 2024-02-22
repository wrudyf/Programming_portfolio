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
    [(Lit l)            (compile-value l)]
    [(Prim0 p)          (compile-prim0 p)]
    [(Prim1 p e)        (compile-prim1 p e)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3)]
    [(Begin e1 e2)      (compile-begin e1 e2)]
    [(Repeat e1 e2)     (compile-repeat e1 e2)]

    ))

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

;; Expr Expr -> Asm
(define (compile-repeat e1 e2)
  ;; TODO

(let ((l1 (gensym 'r1l))
      (l2 (gensym 'r2))
      )
  (seq  (compile-e e1)                ;;compile e1 to see what val it returns in rax (if it doesn't produce an error)
        (assert-integer rax)          ;;make sure the val in rax is an integer
        (Cmp rax (value->bits 0))     ;;make sure that val in rax is not 0
        (Je 'err)                     ;;if equal to 0, jump to label error
        (Add rax (value->bits 1))     ;;add 1 to rax to get proper upper bound
        (Mov 'r9 rax)                 ;;move i value into r9
        (Label l2)                    ;;loop label

        (Cmp 'r9 (value->bits 1))     ;;check to see if register is equal to 1

        (Je l1)                       ;;if equal to 1, jump to end label
        (Push 'r9)                    ;;put i val onto stack
        (compile-e e2)                ;;compile e2
        (Pop 'r9)                     ;;get i value from stack back into rax
        (Sub 'r9 (value->bits 1))     ;;subtract i by 1
        (Jmp l2)                      ;;jump back to start of loop

        (Label l1)                    ;;end
  )
))
