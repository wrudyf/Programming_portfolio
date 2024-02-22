#lang racket
(provide Lit Prim0 Prim1 If Begin Begin0)

;; type Expr =
;; | (Lit Datum)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; | (Begin0 Expr Expr) ;; NEW

;; type Datum =
;; | Eof
;; | Integer
;; | Boolean
;; | Character

;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
(struct Lit (d)          #:prefab)
(struct Prim0 (p)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Begin (e1 e2)    #:prefab)
(struct Begin0 (e1 e2)   #:prefab)
