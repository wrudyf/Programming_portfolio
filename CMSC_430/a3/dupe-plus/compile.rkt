#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)
(require "parse.rkt")

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim1 p e)       (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    [(Cond cs e)       (compile-cond cs e)]
    [(Case e cs el) (compile-case e cs el)]
    ))


;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (value->bits i))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (value->bits b))))

;;can use registers r9 to r13
;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
       ;;will i need to push onto stack for primitive operations here?
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ;; TODO: Handle abs, -, and not

         ['abs
            (let ((l1 (gensym 'nabs)))
            (seq (Cmp 'rax (value->bits 0))
                 (Jg l1)
                 ;;will i need to push onto stack?
                 (Mov 'r9 'rax)
                 (Mov 'rax (value->bits 0))
                 (Sub 'rax 'r9)
                 (Label l1)))
            ]


         ['- (seq (Mov 'r9 (value->bits 0))
                  (Sub 'r9 'rax)
                  (Mov 'rax 'r9)
         )]

;;falsiness, remember everything except #f is #t, so 2 is #t, '() is #t, only thing #f is #f
         ['not
         (let ((l1 (gensym 'ncf))
              (l2 (gensym 'nen))
              )
            (seq  (Cmp 'rax val-false)
                  (Je l1)
                  (Mov 'rax val-false)
                  (Jmp l2)
                  (Label l1)
                  (Mov 'rax val-true)
                  (Label l2)))
         ]

         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))])))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

(define (compile-clause a)
(match a
[(Clause b c)
  (let ( (l1 (gensym 'clausecs))
      )
    (seq  (compile-e b)
          (Cmp 'rax val-false)
          (Je l1)
          (compile-e c)
          (Label l1)
    )
  )]
)
)

(define (compile-cond-cs cs tl)
  (match cs
  ['() (seq (Mov 'rax (value->bits #f)))]
  ;;have to match on clause here
  [(cons a b)
          (seq  (compile-clause a)      ;;compile clause to get val in rax
                (Cmp 'rax val-false)    ;;see if rax has false value
                (Jne tl)                ;;if not false, jump to end label passed in

                                        ;;why does it jump to first label that's true?
                                        ;;subsequent labels can also be true, but once you
                                        ;;jump, that's it, it doesn't check the other labels
                                        ;;but it still has that code for other conds after
                                        ;;first true

                (compile-cond-cs b tl)  ;;continue to compile other instructions
          )
  ]
  )
)

(define (compile-cond cs e)
    (let ((l1 (gensym 'ccs1))
          (l2 (gensym 'ccs2))
          )
    (seq ;;idea, compile all items in cs list and first one that returns true, jump to true label
          (compile-cond-cs cs l2)
    (Label l1)    ;;compile e if no conds were true
          (compile-e e)
    (Label l2)    ;;end
    )

    )
  )

(define (compile-case-list x tl b)
(match x
['() (seq)]
[(cons c d)
(let ((l1 (gensym 'xl))
      (l2 (gensym 'xle))
      (l3 (gensym 'xle1))
      )
(seq    (Mov 'r11 'rax)                    ;;move rax val into r11 for temporary storage
        (compile-e (parse c))              ;;compile first elem in list and get val in rax
        (Cmp 'rax 'r11)                    ;;compare rax to r11 to see if equal
        (Jne l1)                             ;;if equal, jump to label l1
        (compile-e b)                      ;;compile b to get clause val in rax
        (Jmp tl)                           ;;jump to end label in original compile case
        (Label l1)
        (Mov 'rax 'r11)                    ;;if not equal, then mov e val back into 'rax
        (compile-case-list d tl b)         ;;compile the rest of the list if not equal



)
)
]
)
)

;;x is clause, t is label
(define (compile-case-clause x tl)
(match x
['() (seq)]
[(Clause a b)
(seq (compile-case-list a tl b)            ;;compile list and pass in end label and b for clause
)
]
))


;;cs is clause list, tl is label
(define (compile-case-cs cs tl)
 (match cs
 ['() (seq)]
 [(cons a b)
        (seq  (compile-case-clause a tl)    ;;compile first clause elem in list of clauses
              (compile-case-cs b tl)        ;;continue writing instructions
        )]
 )
)


(define (compile-case e cs el)
  (let ((l1 (gensym 'ccase1))
        (l2 (gensym 'ccase2))
        )
  (seq
        ;;idea, compile e to get comp value into rax
        (compile-e e)               ;;compile e to get comparison case in rax, e

        (compile-case-cs cs l2)     ;;compile the list of cs to get any true cases
        (Label l1)
        (compile-e el)              ;;compile else if no conds were true
        (Label l2)                  ;;end condition to jump to if we had a true case
  )
  )
)
