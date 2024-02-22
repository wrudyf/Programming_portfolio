#lang racket
(provide Lit Prim0 Prim1 If Begin Repeat)
;; type Expr =
;; | (Lit Datum)
;; | (Prim1 Op1 Expr)
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; | (Repeat Expr Expr)    ;; NEW
;; type Datum = Integer
;;            | Boolean
;;            | Character
;;            | Eof
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
(struct Lit   (l)        #:prefab)
(struct Prim0 (p)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Repeat (e1 e2)    #:prefab) ;; NEW
(struct Begin (e1 e2)    #:prefab)
