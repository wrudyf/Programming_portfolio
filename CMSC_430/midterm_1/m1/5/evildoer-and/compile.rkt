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
    [(AndE es)     (compile-and es)]  ;; NEW
    [(Int i)       (compile-value i)]
    [(Bool b)      (compile-value b)]
    [(Char c)      (compile-value c)]
    [(Eof)         (compile-value eof)]
    [(Prim0 p)     (compile-prim0 p)]
    [(Prim1 p e)   (compile-prim1 p e)]
    [(If e1 e2 e3) (compile-if e1 e2 e3)]
    [(Begin e1 e2) (compile-begin e1 e2)]))

;; [Listof Expr] -> Asm
(define (compile-andy es)
  (match es
  ['()
  ;;if we reach the end, just return whatever the last value in rax was so don't do anything
  (seq
        (Mov rax rax))]

  [ (cons a b)
  ;;evaluate the first element a
    (let ((l1 (gensym 't1))
        (l2 (gensym'shortc))
        )
    (seq  (compile-e a) ;;compile first elem and check what val is in rax
          (Cmp rax val-false)    ;;check to see if we have false in rax, short circuit if false
          (Je l2)                 ;;jump to stop further code gen/compilation
          (compile-andy b)        ;;if not false, just continue compiling to get more code
          (Label l2)
    )
    )]

  )
)

(define (compile-and es)
  ;; TODO
  ;;only need this initial case to see if we ONLY get "and" <- just return true there
  (match es
  ['()
  (seq (Mov rax val-true)
  )
  ]
  [ _ (seq (compile-andy es))])
)

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
         (Cmp rax val-false)
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
