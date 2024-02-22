#lang racket
(provide symbol->label
         (all-from-out a86/ast))
(require a86/ast)

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
          (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
