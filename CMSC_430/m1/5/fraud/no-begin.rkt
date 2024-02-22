#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Expr -> Expr
(define (no-begin e)

  (match e
    [(Lit l)            (Lit l)]
    [(Var x)            (Var x)]
    ;; TODO
    ;;this should mostly be the same, we will produce another ast and do no-begin on each expression
    ;;however, the only thing that should change is begin
    [(Prim0 p)          (Prim0 p)]

    [(Prim1 p e1)       (Prim1 p (no-begin e1))]

    [(Prim2 p e1 e2)    (Prim2 p (no-begin e1) (no-begin e2))]

    [(If e1 e2 e3)      (If (no-begin e1) (no-begin e2) (no-begin e3))]

    ;;let does about the same thing as begin, where it compiles e1 and then compiles e2, so we just have to convert
    ;;begin node to let node by just making a let node with a gensym for the symbol for x
    [(Begin e1 e2)      (Let (gensym 'x) (no-begin e1) (no-begin e2))]

    [(Let x e1 e2)      (Let x (no-begin e1) (no-begin e2))]

  ))

