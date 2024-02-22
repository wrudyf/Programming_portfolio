# CMSC 430 Midterm 2, Part 3


## Instructions

You've been provided a modified implementation of the Hoax language that was
presented in class.

We've extended the functionality of Hoax to include a new `vector-copy*`
function. This function is similar to Racket's `vector-copy`, but with some
minor differences (described in the Notes below). It works as follows:

  * The syntax is: `(vector-copy* e1 e2)`
      * `e1` should evaluate to a vector, _v_.
      * `e2` should evaluate to an integer, _n_.
      * The result of evaluation is a new vector of length _n_, which is made by
        copying the first _n_ elements out of _v_.
      * If vector _v_ does not have _n_ elements, an error is raised.

Unfortunately, although we tested our interpreter implementation, we didn't test
our compiler implementation very well and we think it has a major flaw!

We need your help in two ways:

  * **TASK 1**: Identify the problem and write a program demonstrating the
    problem in the `vector-copy-flaw.rkt` file.

  * **TASK 2**: Fix the implementation to remove the flaw by modifying
    `compile-ops.rkt`.

There are two primary means by which you can discover the flaw:

  1. Write tests according to the description above.

  2. Read the new code in `compile-ops.rkt` directly.

We have no preference for how you go about it.

A program that correctly identifies the flaw is one that runs successfully with
the interpreter but diverges in the compiler by returning a different value than
the interpreter.

**IMPORTANT**: It is possible to write a program that causes the compiler to
_crash_ rather than return an incorrect result. While this is technically
correct, the autograder will be unable to grade submissions of this nature. That
said, once such a program is found, it should be easy to modify it to return an
incorrect result instead of crashing. If you have found a program that crashes
the compiler, but you cannot think of how to modify it to return an incorrect
result, submit a private Piazza post with your program and we will help you make
the minimal change required so the submission will not crash.


## Notes

  * Our `vector-copy*` is similar to Racket's `vector-copy`, but not identical.
    In `vector-copy` you supply the start and end points instead of a number of
    elements, i.e., `(vector-copy* e1 n)` is equivalent to `(vector-copy e1 0
    n)`.

  * We have provided new tests in `test/vector-copy-test-runner.rkt`. However,
    all of these tests pass --- they do not demonstrate the flaw. You can use
    this file to experiment on your own.

  * The interpreter implementation is guaranteed to be correct, and the compiler
    implementation is guaranteed to be incorrect. This is not a trick question.
