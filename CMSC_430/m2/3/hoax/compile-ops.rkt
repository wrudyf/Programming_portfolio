#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-op3 pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require "../../utility/a86/ast.rkt")

(define rax 'rax)
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in op2
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch

(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq pad-stack (Call 'read_byte) unpad-stack)]
    ['peek-byte (seq pad-stack (Call 'peek_byte) unpad-stack)]))

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
                     if-equal)]
    ['char?
               (seq (And rax mask-char)
                    (Cmp rax type-char)
                    if-equal)]
    ['char->integer
               (seq (assert-char rax)
                    (Sar rax char-shift)
                    (Sal rax int-shift))]
    ['integer->char
               (seq (assert-codepoint)
                    (Sar rax int-shift)
                    (Sal rax char-shift)
                    (Xor rax type-char))]
    ['eof-object?
               (seq (Cmp rax (value->bits eof))
                    if-equal)]
    ['write-byte
               (seq assert-byte
                    pad-stack
                    (Mov rdi rax)
                    (Call 'write_byte)
                    unpad-stack)]

    ['box
               (seq (Mov (Offset rbx 0) rax) ; memory write
                    (Mov rax rbx)            ; put box in rax
                    (Or rax type-box)        ; tag as a box
                    (Add rbx 8))]

    ['unbox
               (seq (assert-box rax)
                    (Xor rax type-box)
                    (Mov rax (Offset rax 0)))]
    ['car
                (seq (assert-cons rax)
                     (Xor rax type-cons)
                     (Mov rax (Offset rax 8)))]
    ['cdr
                (seq (assert-cons rax)
                     (Xor rax type-cons)
                     (Mov rax (Offset rax 0)))]

    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons? (type-pred ptr-mask type-cons)]
    ['box?  (type-pred ptr-mask type-box)]
    ['vector? (type-pred ptr-mask type-vect)]
    ['string? (type-pred ptr-mask type-str)]
    ['vector-length
                (let ((zero (gensym))
                      (done (gensym)))
                  (seq (assert-vector rax)
                       (Xor rax type-vect)
                       (Cmp rax 0)
                       (Je zero)
                       (Mov rax (Offset rax 0))
                       (Sal rax int-shift)
                       (Jmp done)
                       (Label zero)
                       (Mov rax 0)
                       (Label done)))]
    ['string-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Xor rax type-str)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]))


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
          if-lt)]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-equal)]
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    ['make-vector ;; size value
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8) ;; r8 = size
            (assert-natural r8)         ;;make sure r8 is greater or equal to 0
            (Cmp r8 0) ; special case empty vector
            (Je empty)   ;; if empty jump to empty label

            (Mov r9 rbx)           ;;mov heap pointer from rbx into r9
            (Or r9 type-vect)      ;;tag as a vector type

            (Sar r8 int-shift)          ;;shift r8 to be integer size
            (Mov (Offset rbx 0) r8)     ;;move into memory (heap) the size of vector
            (Add rbx 8)                 ;;increase heap pointer

            (Label loop)                ;;LOOP label here <<<<<<------------------------------
            (Mov (Offset rbx 0) rax)    ;;move rax val into memory
            (Add rbx 8)                 ;;add 8 so we can move to next avail spot in heap
            (Sub r8 1)                  ;;subtract vector size by 1
            (Cmp r8 0)                  ;;check to see if vector size is 0 to move on
            (Jne loop)                  ;;if not 0, jump back to loop

            (Mov rax r9)                ;;mov heap pointer into rax
            (Jmp done)                  ;;jump to finish label

            (Label empty)               ;;EMPTY label here  <<<<<-----------------------------
            (Mov rax type-vect)         ;;tag as a vector type
            (Label done)))]             ;;DONE label here   <<<<<-----------------------------

    ['vector-ref ; vector index
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp r8 type-vect)
          (Je 'err) ; special case for empty vector
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]
    ;; NOTE: New.
    ;; TODO: Find the flaw!
    ['vector-copy*
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)                    ; r8 = original vector from stack


            (assert-vector r8)          ;;make sure r8 is a vector

            (Xor r8 type-vect)          ;;remove vector tag


            (assert-natural rax)        ; rax = size
            (Cmp rax 0)                 ; special case empty result vector
            (Je empty)

            ;;before moving to first element, first check to see if original vector size is as big as
            ;;the number e2 evaluates to in vector-copy
            (Sar rax int-shift)
            (Mov 'r11 (Offset r8 0));
            (Cmp rax 'r11)                ;;compare size in rax to size in original vector
            (Jg 'err)                   ;;if rax is greater than r8, we jump to err label in compile.rkt
            (Add r8 8)                  ; move r8 to first element...



            (Mov r9 rbx)                ; r9 = new result vector, move heap pointer into r9
            (Or r9 type-vect)           ;;tag r9 as a vector
;;            (Sar rax int-shift)         ;;do int shift on number in rax (num of elements to copy)
            (Mov (Offset rbx 0) rax)    ;;move into memoery the size of the vector
            (Add rbx 8)                 ;;increase heap pointer so we can add elems to vector

            (Label loop)                ; loop [rax] times
            (Mov r10 (Offset r8 0))     ; copy value in original vector
            (Mov (Offset rbx 0) r10)    ; write value to new result vector
            (Add rbx 8)                 ; increment heap pointer
            (Add r8  8)                 ; increment original vector pointer
            (Sub rax 1)                 ; decrement counter
            (Cmp rax 0)                 ; check if counter is depleted
            (Jne loop)

            (Mov rax r9)                ; return new result vector
            (Jmp done)

            (Label empty)               ; return empty result vector
            (Mov rax type-vect)
;;i think the major flaw is that we don't check to see if original vector has n elements to copy
            (Label done)))]


    ['make-string
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)
            (Cmp r8 0) ; special case empty string
            (Je empty)

            (Mov r9 rbx)
            (Or r9 type-str)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Sar rax char-shift)

            (Add r8 1) ; adds 1
            (Sar r8 1) ; when
            (Sal r8 1) ; len is odd

            (Label loop)
            (Mov (Offset rbx 0) eax)
            (Add rbx 4)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-str)
            (Label done)))]
    ['string-ref
     (seq (Pop r8)
          (assert-string r8)
          (assert-integer rax)
          (Cmp r8 type-str)
          (Je 'err) ; special case for empty string
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 2)
          (Add r8 rax)
          (Mov 'eax (Offset r8 8))
          (Sal rax char-shift)
          (Or rax type-char))]))

;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r10 0)
          (Jl 'err)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Offset r8 0))   ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'err)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax (value->bits (void))))]))


;; -> Asm
;; set rax to #t or #f if comparison flag is equal
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

;; -> Asm
;; set rax to #t or #f if comparison flag is less than
(define if-lt
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmovl rax r9)))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'err))))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       if-equal))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-vector
  (assert-type ptr-mask type-vect))
(define assert-string
  (assert-type ptr-mask type-str))

(define (assert-codepoint)
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (value->bits 0))
         (Jl 'err)
         (Cmp rax (value->bits 1114111))
         (Jg 'err)
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))

(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (value->bits 0))
       (Jl 'err)
       (Cmp rax (value->bits 255))
       (Jg 'err)))

(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'err)))

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
