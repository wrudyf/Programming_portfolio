#lang racket
(provide AndE Eof Int Bool Char Prim0 Prim1 If Begin)

;; NOTE: the name AndE is chosen to avoid a conflict with the a86
;; instruction And.

;; type Expr =
;; | (AndE [Listof Expr]) ;; NEW
;; | (Eof)
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
(struct AndE  (es)       #:prefab) ;; NEW
(struct Eof   ()         #:prefab)
(struct Int   (i)        #:prefab)
(struct Bool  (b)        #:prefab)
(struct Char  (c)        #:prefab)
(struct Prim0 (p)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Begin (e1 e2)    #:prefab)
