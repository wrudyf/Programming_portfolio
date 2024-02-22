#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)
           (Push r15)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           (Pop r15)
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  (match fun
    [(FunPlain xs e)
     (seq (Label (symbol->label f))
          ;; TODO: check arity, r8 has args size
          (compile-e (Int (length xs)) '())     ;;get size into rax
          (Cmp rax 'r8)                         ;;compare args size to defined size
          (Jne 'raise_error_align)              ;;if not equal, then error

          (compile-e e (reverse xs))            ;;otherwise, compile fun
          (Add rsp (* 8 (length xs)))           ;;pop off all args from stack
          (Ret))]

    ;; TODO: handle other kinds of functions
    [(FunRest xs x e)
    ;;xs is args, x is the rest argument, e is expression
    ;;the rest arguments are put on the heap
    ;;all args on stack BECAUSE OF COMPILE-APP
    ;;put the extra args on the heap
    ;;return a pointer to the list

    (let ((l1 (gensym 'x))
          (l2 (gensym 'y))
          (env (if (empty? xs) (list x)   (cons x (reverse xs)) ) )
          )
     (seq (Label (symbol->label f))
          ;; check arity for rest....done i think, remember case for 0 args all go in list
          (compile-e (Int (length xs)) '())     ;;get size of minimum num of args into rax
          (Cmp 'r8 rax)                         ;;compare args given to min args needed
          (Jl 'raise_error_align)               ;;jump to error if args is less than min args

          ;;if we have min args, then do processing below
          (Sub 'r8 rax)                         ;;subtract args given from the minimum number of args needed
                                                ;;num of args - num of expected args = things to pop off the stack

          (Mov rax (value->bits '()))  ;;put empty list on heap

          ;;loop is right
          (Label l1)                              ;;loop label
          (Cmp 'r8 (value->bits 0))               ;;compare r8 to 0
          (Je l2)                                 ;;if equal to 0, jump to exit loop label
          (Mov (Offset rbx 0) rax)                ;;move rax into first heap cons cell
          (Pop rax)                               ;;pop arg off stack and get in rax
          (Mov (Offset rbx 8) rax)                ;;move arg into second half of cons cell
          (Mov rax rbx)                           ;;move heap pointer into rax
          (Or rax type-cons)                      ;;tag as cons
          (Add rbx 16)                            ;;move to next available space in heap

          (Sub 'r8 (value->bits 1))               ;;subtract our counter by 1
          (Jmp l1)                                ;;jump back to top
          (Label l2)                              ;;exit loop label

          (Push rax)
          (compile-e e  env)
          ;;(Add rsp 8)
          (Add rsp (* 8 (length env)))           ;;pop off all args from stack
     (Ret)
     )
     )]

    [(FunCase cs)
      ;;note, cs is list of functions f
      ;;idea, we have a list of functions
      ;;we can compile each function
      ;;and then we can check to see if we have the right number of args, if it is, we execute current function
      ;;if it isn't and we reach end of list, we return an error for "arity mismatch"
      ;;this should be right because we run through each function everytime

      ;;new idea, make one really long function that checks along the way if arities match with diff
      ;;"programs"
      (let ((early_exit (gensym 'exit))
            )
      (seq (Label (symbol->label f))
          (make_long_fun cs early_exit)
          (Label early_exit)
          (Ret)
      )
      )
    ]
     ))

(define (make_long_fun funs exl)
    (match funs
    ['() (seq (Jmp 'raise_error_align))]    ;;if we reach end of list, no matches so arity mismatch, err
    [(cons current_fun rest_funs)
      (match current_fun
        [(FunPlain xs e)
        (let ((l1 (gensym 'xi))
              (l2 (gensym 'yi))
              )
          (seq
            (compile-e (Int (length xs)) '())
            (Cmp rax 'r8)
            (Jne l1)
            (compile-e e (reverse xs))
            (Add rsp (* 8 (length xs)))
            (Jmp exl)
            (Label l1)
            (make_long_fun rest_funs exl)
          )
        )
        ]

        [(FunRest xs x e)
        (let ((l1 (gensym 'xfr))
              (l2 (gensym 'yfr))
              (nx (gensym 'yii))
              (envi (if (empty? xs) (list x) (cons x (reverse xs))) )
              )
              (seq
                  (compile-e (Int (length xs)) '())
                  (Cmp 'r8 rax)
                  (Jl nx)

                  (Sub 'r8 rax)
                  (Mov rax (value->bits '()))

                  (Label l1)                              ;;loop label
                  (Cmp 'r8 (value->bits 0))               ;;compare r8 to 0
                  (Je l2)                                 ;;if equal to 0, jump to exit loop label
                  (Mov (Offset rbx 0) rax)                ;;move rax into first heap cons cell
                  (Pop rax)                               ;;pop arg off stack and get in rax
                  (Mov (Offset rbx 8) rax)                ;;move arg into second half of cons cell
                  (Mov rax rbx)                           ;;move heap pointer into rax
                  (Or rax type-cons)                      ;;tag as cons
                  (Add rbx 16)                            ;;move to next available space in heap

                  (Sub 'r8 (value->bits 1))               ;;subtract our counter by 1
                  (Jmp l1)                                ;;jump back to top
                  (Label l2)                              ;;exit loop label
                  (Push rax)
                  (compile-e e envi)
                  (Add rsp (* 8 (length envi)))
                  (Jmp exl)
                  (Label nx)
                  (make_long_fun rest_funs exl)

              )
        )

        ]

        ;;figure out what to do for funcase
        [(FunCase cs)
          (seq
              (compile-fun (gensym 'f) (cons cs ))
          )
        ]
      )
    ]
))



;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(App f es)         (compile-app f es c)]
    [(Apply f es e)     (compile-apply f es e c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

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

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax (value->bits #f))
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
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)                        ;;load addr of label r into rax
         (Push rax)                         ;;put ret addr on stack
         (compile-es es (cons #f c))        ;;put args on stack
         ;; TODO: communicate argument count to called function
         ;;(Push rax)                         ;;save whatever last arg was in rax on stack

         (compile-e (Int (length es)) '())  ;;get length of args and put in rax
         (Mov 'r8 rax)                      ;;move rax val into r8
         ;;(Pop rax)                          ;;Pop val from stack to get back into rax
         (Jmp (symbol->label f))
         (Label r))))

;; Id [Listof Expr] Expr CEnv -> Asm
;;e should be a list
;;opposite of rest argument function
;;make es and e work and use f to do arity checking

;;idea...first push everything in es onto the stack and also
;;unwrap the list e and put that on the stack too....E IS NOT LIST
;;E IS NOT LIST, IT IS AN EXPRESSION THAT NEEDS TO EVALUATE TO A LIST
(define (compile-apply f es e c)
  ;; TODO: implement apply
  (let ((r (gensym 'ret))
        (l1 (gensym 'x))
        (l2 (gensym 'y))
        (l3 (gensym 'se1))
        (l4 (gensym 've))
        )

    (seq (Lea rax r)                          ;;load address of label r into rax
          (Push rax)                          ;;push that address on the stack
          (compile-es es (cons #f c))         ;;push list of id's onto stack
          ;;OK ABOVE HERE

          (compile-e e c)                     ;;compile e to get list into rax
          (Cmp rax (value->bits '()))         ;;empty list case
          (Je l1)                             ;;jump to case to make empty list
          ;;do comparison to make sure e is actually of type cons if not empty
          (Mov 'r9 rax)                       ;;move rax into r9
          (And 'r9 ptr-mask)
          (Cmp 'r9 type-cons)
          (Jne 'raise_error_align)

          (Mov 'r10 (value->bits 0))          ;;use r10 to keep count of args from cons
          (Label l2)                          ;; loop label
          (Xor rax type-cons)                 ;;remove cons tag
          (Mov 'r8 (Offset 'rax 8))           ;;move value into r8
          (Push 'r8)                          ;;put r8 val onto stack
          (Add 'r10 (value->bits 1))          ;;add 1 to r10 counter
          (Mov rax (Offset 'rax 0))           ;;move pointer into rax
          (Cmp rax (value->bits '()))         ;;compare to see if it's empty list
          (Je l3)                             ;;if it is empty, jump to end loop label
          (Jmp l2)                            ;;jump back to start of loop
          (Label l3)                          ;;end loop label
          (compile-e (Int (length es)) '())   ;;get size of id's
          (Mov 'r8 rax)                       ;;move size of id's into r8
          (Add 'r8 'r10)                      ;;add args from cons to size of id's into r8 to get total args

          (Jmp l4)                            ;;jump to very end

          (Label l1)                          ;;empty case
          (compile-e (Int (length es)) '())   ;;get length of id's into rax
          (Mov 'r8 rax)

          (Label l4)                          ;;very end label
          (Jmp (symbol->label f))             ;;function call here
          (Label r)
        )
  ))
(define (compile-el e c)
  (match e
  ['() (seq)]
  [(cons ec enext)
    (seq (compile-e ec c)
        (Push rax)
        (compile-el enext (cons #f c))
    )
  ]
  [_ (seq (Jmp 'raise_error_align))]
  )
)

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

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
