#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch
(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
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
     (seq (assert-integer rax)
          (Cmp rax 0)
          (if-equal))]
    ['char?
     (type-pred mask-char type-char)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-value eof)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack)]))

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
          (if-lt))]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)          
          (if-equal))]))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error_align))))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       (if-equal)))

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

;; -> Asm
;; set rax to #t or #f based on given comparison
(define (if-compare c)
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (c rax r9)))

(define (if-equal) (if-compare Cmove))
(define (if-lt) (if-compare Cmovl))

;; Value -> Asm
(define (eq-value v)
  (seq (Cmp rax (value->bits v))
       (if-equal)))

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
