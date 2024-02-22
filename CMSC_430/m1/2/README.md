# CMSC 430 Midterm 1, Part 2

## Instructions

You've been provided the implementation of Dupe as presented in class.
No modifications of the interpreter or compiler have been made.

Consider the following Dupe program

```
(add1 #f)
```

You can run this program with the interpreter using the following command:

> cat example.rkt | racket -t interp-stdin.rkt -m

You will see a contract violation from _Racket_, the language we have used to
implement our interpreter.

If you use the compiler and run the resulting program:

> make example.run
> ./example.run

You will see `internal error`.

One might argue that this means the compiler for `Dupe` has a form of type
checking. However, the faculty have argued that we did not have any form of
type-checking until `Extort`.

Write a program in 'example.rkt' that misuses types in this way. When run in
the interpreter, it should produce a Racket contract violation. When run after
being compiled, it should produce a value.

Note: This requires no modification to the compiler/interpreter. All you have
to do is replace the Dupe program in example.rkt with one that exhibits this
behavior.
