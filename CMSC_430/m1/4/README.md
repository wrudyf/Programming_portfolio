# CMSC 430 Midterm 1, Part 4

## Instructions

You've been provided a modified implementation of Evildoer, as presented in class.

Trying to live up to my idol, Sonic the Hedgehog, I've attempted to make our Evildoer
compiler produce programs that are as fast as possible. The only change that I've made
so far is to pre-evaluate certain expressions. I did this by altering `compile-prim1`.
Here is the modified version:

```
;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (let ((v (interp e)))
       (seq (Mov 'rax (value->bits v))
            (compile-op1 p))))
```


However, I'm pretty sure that this is wrong in some way. I need your help in two ways:

* Help me identify the problem by writing an Evildoer program that exhibits the
  bad behavior (place this program in a file called example.rkt)

* Fix the compiler so that `(interp e)` only occurs when it is safe to do so.


Hint: For the second part you'll probably want to write a function that takes
in an AST and returns a bool.
