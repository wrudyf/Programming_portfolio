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
    [(Int i)        (compile-value i)]
    [(Bool b)       (compile-value b)]
    [(Char c)       (compile-value c)]
    [(Eof)          (compile-value eof)]
    [(Prim0 p)      (compile-prim0 p)]
    [(Prim1 p e)    (compile-prim1 p e)]
    [(When e1 e2)   (compile-when e1 e2)] ;; NEW
    [(Unless e1 e2) (compile-unless e1 e2)] ;; NEW
    [(If e1 e2 e3)  (compile-if e1 e2 e3)]
    [(Begin e1 e2)  (compile-begin e1 e2)]))

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
(define (compile-when e1 e2)
  ;; TODO
  ;;idea, compile e1, check rax to see if it has #f value
  ;;if it has #f value, then we move void val into rax and return
  ;;if it is not #f, then we compile e2 and then return

  (let ((l1 (gensym 'whenf))
        (l2 (gensym 'whennotf))
        (l3 (gensym 'final_end))
        )
  (seq  (compile-e e1)              ;;compile e1
        (Cmp rax (value->bits #f))  ;;check to see if evaluating e1 returns #f in rax
        (Je l1)                     ;;
        (compile-e e2)
        (Jmp l3)
        (Label l1)
        (Mov rax val-void)
        (Label l3)
  )))

;; Expr Expr -> Asm
(define (compile-unless e1 e2)
  ;; TODO
  ;; idea, compile e1, check rax to see if it has #f value
  ;; if it has #f value, then jump to evaluate e2 and return that value
  ;; if it is NOT #f value, then return void

  (let    ((l1 (gensym 'unlessf))
          (l2 (gensym 'unlesst))
          (l3 (gensym 'unlessend))
          )
  (seq    (compile-e e1)              ;;compile e1
          (Cmp rax (value->bits #f))  ;check to see if evaluating e1 returns #f in rax
          (Je l1)                     ;;if rax is false, then evaluate e2
          (Mov rax val-void)
          (Jmp l3)
          (Label l1)
          (compile-e e2)
          (Label l3)
  )))

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
