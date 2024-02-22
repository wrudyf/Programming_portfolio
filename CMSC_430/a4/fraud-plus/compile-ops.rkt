#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch in assert-type
(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq pad-stack
                     (Call 'read_byte)
                     unpad-stack)]
    ['peek-byte (seq pad-stack
                     (Call 'peek_byte)
                     unpad-stack)]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (let ((l1 (gensym)))
       (seq (assert-integer rax)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char? (type-pred mask-char type-char)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-imm val-eof)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack
          (Mov rax val-void))]
    ;; TODO: implement -, abs, integer?, boolean?, etc.
    ['-
        (seq      (assert-integer rax)
                  (Mov 'r9 (value->bits 0))
                  (Sub 'r9 'rax)
                  (Mov 'rax 'r9)
         )
    ]
    ['abs      (let ((l1 (gensym 'nabs)))
            (seq  (assert-integer rax)
                  (Cmp 'rax (value->bits 0))
                 (Jg l1)
                 ;;will i need to push onto stack?
                 (Mov 'r9 'rax)
                 (Mov 'rax (value->bits 0))
                 (Sub 'rax 'r9)
                 (Label l1)))]
    ['not      (let ((l1 (gensym 'ncf))
              (l2 (gensym 'nen))
              )
            (seq  ;;(assert-boolean rax)
                  (Cmp 'rax val-false)
                  (Je l1)
                  (Mov 'rax val-false)
                  (Jmp l2)
                  (Label l1)
                  (Mov 'rax val-true)
                  (Label l2)))
    ]
    ['integer? (let ((l1 (gensym 'inte1))
                      (l2 (gensym 'inte2))
                      (l3 (gensym 'inte3))
                    )
                (seq  (Mov 'r10 'rax)           ;;temporarily put val in rax into r10
                      (And 'r10 mask-int)       ;;and the argument to the integer mask
                      (Cmp 'r10 type-int)       ;;check to see if r10 is equal to type inte
                      (Jne l2)                  ;;if not equal, jump to end and move false
                      (Mov 'rax val-true)       ;;move true val into rax
                      (Jmp l3)
                      (Label l2)
                      (Mov 'rax val-false)      ;;move false val into rax
                      (Label l3)
    ))]

    ['boolean? (let ((l1 (gensym 'boool1))
                    (l2 (gensym 'boool2))
                    (l3 (gensym 'boool3))
                    )
                (seq  (Cmp 'rax val-true)       ;;compare current rax value to true
                      (Jne l1)                    ;;if not equal, jump to label to check for #f
                      (Jmp l2)                    ;;jump to end label and don't change reg
                      (Label l1)                  ;;label to check for f
                      (Cmp 'rax val-false)      ;;compare current rax val to false
                      (Jne l3)                    ;;if not equal, jump to label to move false val
                      (Mov 'rax val-true)       ;;if no jump, then equal to false, mov true into rax

                      (Jmp l2)                    ;;jump to end label
                      (Label l3)                  ;;if not equal to #t or #f, then #f
                      (Mov 'rax val-false)
                      (Label l2)                  ;;end label
    ))]

))

;; Op2 -> Asm
(define (compile-op2 p)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sub r8 rax)
          (Mov rax r8))]
    ['<
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)          
          (Mov rax val-true)
          (let ((true (gensym)))
            (seq (Jl true)
                 (Mov rax val-false)
                 (Label true))))]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)          
          (Mov rax val-true)
          (let ((true (gensym)))
            (seq (Je true)
                 (Mov rax val-false)
                 (Label true))))]))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error_align))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (value->bits #t))
         (Je l)
         (Mov rax (value->bits #f))
         (Label l))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define (assert-codepoint r)
  (let ((ok (gensym)))
    (seq (assert-integer r)
         (Cmp r (value->bits 0))
         (Jl 'raise_error_align)
         (Cmp rax (value->bits 1114111))
         (Jg 'raise_error_align)
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp 'raise_error_align)
         (Label ok))))

(define (assert-byte r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'raise_error_align)
       (Cmp r (value->bits 255))
       (Jg 'raise_error_align)))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))
