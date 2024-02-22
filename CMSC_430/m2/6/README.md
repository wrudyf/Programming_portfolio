# CMSC 430 Midterm 2, Part 6


## Instructions

You've been provided a modified implementation of the Knock language that was
presented in class.

We want to add a couple new patterns for our `match` expressions: `?` and `app`.
These patterns will allow us to apply functions to the structures we match
against, though they have slightly different semantics. We describe the patterns
in detail below. In both cases, we are only implementing the functionality for
user-defined functions --- not primitive operations.

The starter code includes an interpreter implementation of `app` and a compiler
implementation of `?`. For this part of the midterm, your task is to implement
the missing parts, i.e., implement the compiler for `app` and the interpreter
for `?`. Some tests have been provided, but the tests are not exhaustive. You
may want to write more tests.


### Predicate Patterns

The `?` pattern applies a predicate to the structure and, if the predicate
returns a non-false value, binds the original structure to a subsequent pattern.
The syntax looks like `(? fun p)`, where `fun` is the name of a function and `p`
is a pattern. An example of the `?` pattern in action might be:

```scheme
(define (two? x)
  (eq? x 2))
(match (- (read-byte) 97)
  [(? two? x) #t]
  [_ #f])
```


### Application Patterns

The `app` pattern applies a function to the structure being matched, and then
binds the result of that application to a subsequent pattern. The syntax looks
like `(app fun p)`, where `fun` is the name of a function and `p` is a fresh
pattern variable. A (silly) example of this pattern in action might be:

```scheme
(define (cons-with-7 x)
  (cons x 7))
(match (- (read-byte) 97)
  [(app cons-with-7 (cons x y)) y]
  [_ #f])
```


## Notes

  * Our implementations of these new patterns only work for a single argument
    pattern, whereas Racket's supports arbitrarily many. For example, Racket
    allows you to use `?` without any subordinate patterns, e.g., `(? two?)`
    would be valid. We don't allow this.

  * Remember that you only need to support user-defined functions, _not_
    primitive operations.

  * We have added tests in `test/new-patterns-test-runner.rkt`. We recommend
    writing tests of your own there.

  * This version of Knock uses the same parser as the version of Iniquity in
    Part 4 did, so refer to that README for usage notes.


## Different AST Nodes

This version of Knock may differ from the version some of you saw in class.
Specifically, you may find different AST nodes for patterns. The new version
(this version) is a bit simpler when it comes to parsing, but we maintain all of
the same features. We believe there should be no cause for concern if you are
familiar with Knock's patterns, no matter what names you know them by.
