# CMSC 430 Midterm 1, Part 3

## Instructions

You've been provided an implementation of Evildoer as presented in
class.

Your job is to implement a new expression form:

- `(begin0 e1 e2)`

`(begin0 e1 e2)` evaluates e1 and then evaluates e2; the value of the whole
expression is the value of e1.

- An example:

   * `(begin0 (begin (write-byte 99) 42) (write-byte 98))`
     evaluates to 42, with 'cb' being written to stdout.

The AST type definition, parser, and interpreter have been updated to
implement these new forms, and a stub has been added to the
compiler. You must finish the implementation in the compiler.

Note: `begin0` is available in Racket, so if you want to gain an intuition
regarding its behavior, you can use it directly in Racket.

Note: there are no tests included, so you should consider writing some of your
own.

Hint: Like all recursive expression forms, `begin0` can be nested to any depth.
As such, you'll want to avoid using registers to store values across the
evaluation of arbitrary expressions.
