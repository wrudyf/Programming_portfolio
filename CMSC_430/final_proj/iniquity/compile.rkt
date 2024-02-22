#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r15 'r15) ; stack pad (non-volatile)

;; type CEnv = (Listof [Maybe Id])
  
;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register
           (Push r15)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
            ;;note, if last thing we compiled is a values, we will want to construct a "vector"
            ;;from those values
           (compile-finalv '())

           (Pop r15)     ; restore callee-save register
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
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))

(define (compile-finalv x)
(let  ((l1 (gensym 'x))
       (l11 (gensym 'xx))
       (l12 (gensym 'xy))
       (l2 (gensym 'y))
       (l3 (gensym 'z))
      )
      ;;ok, good
(seq
    (Mov 'r8 rax)                               ;;move rax val into r8
    (And 'r8 ptr-mask)                          ;;mask all the bits to just get the last 3 to see if it's pointer type
    (Cmp 'r8 type-values)                       ;;check to see if what we have is of type values

    (Je l1)                                     ;;if equal, jump to label to create vector
    (Jmp l2)                                    ;;if it wasn't equal, then we don't have a type values and we don't need to create vector vals

    ;;scratch registers..r8, r9, r10

    (Label l1)                                  ;;label to create vec
    (Mov 'r8 'r9)                               ;;moving args to r8
    (Sar 'r8 int-shift)                         ;;get size
    (Mov (Offset rbx 0) 'r8)                    ;;write size of our vector, so info in r8 can be overwritten now
    (Mov 'r9 rbx)                               ;;use r9 to hold rbx pointer
    (Add rbx 8)

    (Label l11)                                 ;;loop label

    (Xor rax type-values)                       ;;remove values tag
    (Mov 'r8 (Offset 'rax 8))
    (Mov (Offset rbx 0) 'r8)
    (Add rbx 8)
    (Mov rax (Offset 'rax 0))                   ;;move pointer into rax
    (Cmp rax (value->bits '()))

    (Je l12)
    (Jmp l11)
    (Label l12)                                 ;;end loop label
    (Mov rax 'r9)                               ;;move start of rbx pointer to rax
    (Jmp l3)

    (Label l2)                                  ;;compile vector size one label
    (Mov 'r8 1)                                 ;;vector size is 1
    (Mov (Offset rbx 0) 'r8)                    ;;write size of vector
    (Mov (Offset rbx 8) rax)                    ;;write rax as single element of vector
    (Mov rax rbx)                               ;;return pointer to vector
    (Label l3)                                  ;;very end label

)
))
;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)              (compile-value i)]
    [(Bool b)             (compile-value b)]
    [(Char c)             (compile-value c)]
    [(Eof)                (compile-value eof)]
    [(Empty)              (compile-value '())]
    [(Var x)              (compile-variable x c)]
    [(Str s)              (compile-string s)]
    [(Prim0 p)            (compile-prim0 p c)]
    [(Prim1 p e)          (compile-prim1 p e c)]
    [(Prim2 p e1 e2)      (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3)   (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)        (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)        (compile-begin e1 e2 c)]
    [(Let x e1 e2)        (compile-let x e1 e2 c)]
    [(App f es)           (compile-app f es c)]
    [(Values es)          (compile-values es c)]
    [(LetValues xs es e)  (compile-letvalues xs es e c)]
))

(define (compile-letvalues xs es e c)
  (let ((l1 (gensym 'x))
        (l2 (gensym 'y))

        )
  (seq
        (compile-e es c)                  ;;es is parsed as a values, so parse those vals

        (Push rax)                        ;;push pointer to vals list onto stack

        (compile-e (Int (length xs)) '()) ;;get length of id's into rax
        (Cmp rax 'r9)                     ;;compare rax to r9 to see if they have the same num of args

        (Jne 'raise_error_align)          ;;if not equal, then jump to raise error align

        (Pop rax)                         ;;get values list back into rax if no arity error was given


        (Label l1)                        ;;loop label
        (Xor rax type-values)             ;;remove values tag
        (Mov 'r9 (Offset 'rax 8))         ;;move value into r9
        (Push 'r9)                        ;;move value onto stack
        (Mov rax (Offset 'rax 0))         ;;move pointer into rax
        (Cmp rax (value->bits '()))       ;;compare rax val to empty list val
        (Je l2)                           ;;if empty value, then jump to end of loop
        (Jmp l1)
        (Label l2)                        ;;end loop label

        (compile-e e (reverse (append xs c)) )
        (Add rsp (* 8 (length xs)))

  )
))

(define (compile-values es c)
  (let  ((l1 (gensym 'x))
         (l2 (gensym 'y))
          )
  (seq  (compile-es es c)                 ;;push all args on stack first
        (compile-e (Int (length es)) '())       ;;get number of values into rax
        ;;idea, make something similar to vector where we end up with
        ;;something in the stack that points to a "list" in the heap
        (Mov 'r8 rax)                     ;;move number of values into r8
        (Mov rax (value->bits '()))       ;;move empty value into rax, empty value will tell us this is the end of vals
        (Mov 'r9 (value->bits 0))         ;;use r9 register to communicate num of args

        (Label l1)                        ;;loop label
        (Cmp 'r8 (value->bits 0))         ;;check to see if we have reached end of loop to process values
        (Je l2)                           ;;jump to end loop label
        (Mov (Offset rbx 0) rax)          ;;move rax val into heap
        (Pop rax)                         ;;get val off stack and put in rax
        (Mov (Offset rbx 8) rax)          ;;move val into next available heap space
        (Mov rax rbx)                     ;;move addr from rbx pointer into rax
        (Or rax type-values)              ;;tag this as a type for values
        (Add rbx 16)                      ;;go to next available space in heap
        (Sub 'r8 (value->bits 1))         ;;subtract 1 from our counter
        (Add 'r9 (value->bits 1))         ;;add 1 for num of args
        (Jmp l1)                          ;;jump back to loop
        (Label l2)                        ;;end loop label

        #|
        ;;ok, everything should be done here now
        (Xor rax type-values)

        ;;(Mov rax (Offset 'rax 8)) ;;value
        (Mov rax (Offset 'rax 0)) ;;mem addr

        (Xor rax type-values)

        (Mov rax (Offset 'rax 0))  ;;mem addr

        (Xor rax type-values)

        (Mov rax (Offset 'rax 8)) ;;value 3|#

  )
))

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

(define (check_one x)
    (let ((l1 (gensym 'a))
          (le (gensym 'b))
          )
      (seq
        (Mov r10 rax)           ;;move rax val into r9
        (And r10 ptr-mask)      ;;get the last 3 digits to check if it's type value
        (Cmp r10 type-values)   ;;compare to see if r9 is the same as values
        (Jne le)               ;if not the same, then jump to end
        ;;if the same, then do the processing below
        (Cmp r9 (value->bits 1))  ;;check to make sure we have one arg
        (Jne 'raise_error_align)  ;;if we don't, then just raise error
        (Xor rax type-values)                 ;;remove value tag
        (Mov rax (Offset 'rax 8)) ;;move value into rax
        (Label le)
      )
    )
)
;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (check_one '())
       (compile-op1 p)))


;;idea, check to see if we have 2 args from our vals
;;if we have 2 args, then we will
;;(define (check_two x)


;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
        (check_one '())
       (Push rax)
       (compile-e e2 (cons #f c))
        (check_one '())
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
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (Jmp (symbol->label f))
         (Label r))))

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
