# CMSC 430 Midterm 2, Part 5


## Instructions

You've been provided a modified implementation of the Jig language that was
presented in class.

In a correct implementation of Jig, `compile-let` looks like this:

```scheme
;; Id Expr Expr CEnv Boolean -> Asm
(define (compile-let x e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons x c) t?)
       (Add rsp 8)))
```

But in this version of Jig, a mistake was made when writing `compile-let`:

```scheme
;; Id Expr Expr CEnv Boolean -> Asm
(define (compile-let x e1 e2 c t?)
  (seq (compile-e e1 c t?)
       (Push rax)
       (compile-e e2 (cons x c) t?)
       (Add rsp 8)))
```

Your task in this part is to recognize what was changed and write a program in
`compile-let-flaw.rkt` that demonstrates the issue. Specifically, you must write
a program that would return `13` in a correct Jig implementation but returns
`87` here instead.


## Notes

  * We have not added any new tests. However, we have added a new file for you
    to write tests in for your convenience: `test/faulty-let-test-runner.rkt`.

  * This version of Jig uses the same parser as the version of Iniquity in Part
    4 did, so refer to that README for usage notes.
