#lang racket
(provide Lit Prim0 Prim1 Prim2 If Begin Let Var)

;; type Expr =
;; | (Lit Datum)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; | (Let Id Expr Expr)
;; | (Var Id)
;; type Datum =
;; | Eof
;; | Integer
;; | Boolean
;; | Character

;; type Id  = Symbol
;; type Op0 = 'read-byte
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;; type Op2 = '+ | '- | '< | '=
(struct Lit (d)          #:prefab)
(struct Prim0 (p)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Begin (e1 e2)    #:prefab)
(struct Let   (x e1 e2)  #:prefab)
(struct Var   (x)        #:prefab)
