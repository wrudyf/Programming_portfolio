#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)
(require "parse.rkt")
;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = (Listof ID)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)    (compile-prim-n1 p es c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let idlst exlst e)
     (compile-letr idlst exlst e c 0)]
     ;;(compile-let1 x e1 e2 c)]
    ;; TODO: implement let*, case, cond
    [(Let* xs es e)  (compile-let* xs es e c 0)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c) ]))

(define (compile-let* idl exl e c l)
(match exl
['() (seq (compile-e e c) (Add rsp (* 8 l)) )]
[(cons x y)
      (match x
      [ (Var x) (seq  (compile-e (Var x) c)
                      (Push rax)
                      (compile-let* (rest idl) y e (cons (first idl) c) (+ l 1))
      )
      ]

      [ _  (seq   (compile-e x c)           ;;compile first e(that isn't var), get into rax
                  (Push rax)                ;;push val of rax onto stack
                  (compile-let* (rest idl) y e (cons (first idl) c) (+ l 1))
          )
      ]
      )
]
))

(define (compile-letr idl exl e c l)
(match idl
['() (seq (compile-e e c) (Add rsp (* 8 l) ))]
[(cons x y)
        (seq  (compile-e (first exl) c)                         ;;compile first e, get into rax
              (Push rax)                                        ;;push val of rax onto stack
              (compile-letr y (rest exl) e (cons x c) (+ l 1))  ;;call fun with updated env and l of list
        )
]
))

;;es is list of expr, so recursively compile each expression of list
(define (compile-prim-n1 p es c)
(match es
['() (seq)]
[(cons a '()) (seq (compile-e a c))]     ;;compile only one case if only one case
[(cons a b)
(seq
              (compile-e a c)           ;;compile 1st elem
              (Push rax)                ;;save val on stack
              (compile-e (first b) (cons #f c))   ;;compile 2nd elem
              (compile-op2 p)
              (compile-prim-n p (rest b) (cons #f c))   ;;compile

)
]
))

(define (compile-prim-n p es c)
(match es
['() (seq) ]

[(cons a b)
(seq
        (Push rax)              ;;push rax val into stack
        (compile-e a c)         ;;compile first value from list...so res is in rax



        (compile-op2 p)         ;;perform necessary operation, + -, < or =

        (compile-prim-n p b (cons #f c))   ;;then generate assembly code for rest of the other elems

;;        (compile-e a c)
;;        (Push rax)
;;        (compile-e (first b) (cons #f c))
;;        (compile-op2 p)
        ;;(compile-primn p (rest b) (cons #f c))
        )
]
))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))


;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))




;;-------------------------------------------->>>COND HERE
(define (compile-clause a c)
(match a
[(Clause b c2)
  (let ( (l1 (gensym 'clausecs))
      )
    (seq  (compile-e b c)
          (Cmp 'rax val-false)
          (Je l1)
          (compile-e c2 c)
          (Label l1)
    )
  )]
))

(define (compile-cond-cs cs tl c)
  (match cs
  ['() (seq (Mov 'rax (value->bits #f)))]
  ;;have to match on clause here
  [(cons a b)
          (seq  (compile-clause a c)    ;;compile clause to get val in rax
                (Cmp 'rax val-false)    ;;see if rax has false value
                (Jne tl)                ;;if not false, jump to end label passed in

                                        ;;why does it jump to first label that's true?
                                        ;;subsequent labels can also be true, but once you
                                        ;;jump, that's it, it doesn't check the other labels
                                        ;;but it still has that code for other conds after
                                        ;;first true

                (compile-cond-cs b tl c)  ;;continue to compile other instructions
          )
  ]
))

(define (compile-cond cs e c)
    (let ((l1 (gensym 'ccs1))
          (l2 (gensym 'ccs2))
          )
    (seq ;;idea, compile all items in cs list and first one that returns true, jump to true label
          (compile-cond-cs cs l2 c)
    (Label l1)    ;;compile e if no conds were true
          (compile-e e c)
    (Label l2)    ;;end
    )
))

;;---------------------------------------------------->>>> CASE HERE

(define (compile-case-list x tl b c2)
(match x
['() (seq)]
[(cons c d)
(let ((l1 (gensym 'xl))
      (l2 (gensym 'xle))
      (l3 (gensym 'xle1))
      )
(seq    (Mov 'r11 'rax)                    ;;move rax val into r11 for temporary storage
        (compile-e (parse c) c2)              ;;compile first elem in list and get val in rax
        (Cmp 'rax 'r11)                    ;;compare rax to r11 to see if equal
        (Jne l1)                             ;;if equal, jump to label l1
        (compile-e b c2)                      ;;compile b to get clause val in rax
        (Jmp tl)                           ;;jump to end label in original compile case
        (Label l1)
        (Mov 'rax 'r11)                    ;;if not equal, then mov e val back into 'rax
        (compile-case-list d tl b c2)         ;;compile the rest of the list if not equal



)
)
]
)
)

;;x is clause, t is label
(define (compile-case-clause x tl c2)
(match x
['() (seq)]
[(Clause a b)
(seq (compile-case-list a tl b c2)            ;;compile list and pass in end label and b for clause
)
]
))


;;cs is clause list, tl is label
(define (compile-case-cs cs tl c2)
 (match cs
 ['() (seq)]
 [(cons a b)
        (seq  (compile-case-clause a tl c2)    ;;compile first clause elem in list of clauses
              (compile-case-cs b tl c2)        ;;continue writing instructions
        )]
 )
)


(define (compile-case e cs el c2)
  (let ((l1 (gensym 'ccase1))
        (l2 (gensym 'ccase2))
        )
  (seq
        ;;idea, compile e to get comp value into rax
        (compile-e e c2)               ;;compile e to get comparison case in rax, e

        (compile-case-cs cs l2 c2)     ;;compile the list of cs to get any true cases
        (Label l1)
        (compile-e el c2)              ;;compile else if no conds were true
        (Label l2)                  ;;end condition to jump to if we had a true case
  )
  )
)
