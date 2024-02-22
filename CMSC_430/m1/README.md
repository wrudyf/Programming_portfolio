# CMSC 430 Midterm 1

## Instructions

There are SIX parts to this midterm.  Each part has its own directory
with a README and supplementary files.  Read the README in each part
for instructions on how to complete that part of the midterm.

## Communications

If you have questions about the exam, post a _private_ question on Piazza.
Ensure that all course staff are able to see the post.

Answers to common clarifying questions will be posted more widely by the course
staff.

You may not communicate with anyone outside of the course staff about the
midterm. Doing the midterm together, as a group, will result in a referral to
the OSC.

## Submissions

You should submit your work as a single zip file of this directory on
Gradescope.

To create a submission zip file, do the following from within the
`m1/` directory:

```
> make submit.zip
```
or simply:
```
make
```
Then submit `submit.zip` on Gradescope.

Unlike past assignments, Gradescope will only do a basic test for
well-formedness of your submission.  It will make sure the directory
layout is correct and that all the functions that will be tested are
available.  It will catch syntax errors in your code, but it does not
run any correctness tests.

If you fail these tests, we will not be able to grade your submission.
Passing these tests only means your submission is well-formed.  Your
actual grade will be computed after the deadline.

You are encouraged to check your own work.

A note on the form of the AST:

In some lectures we do not use the AST node `Lit` as it is in this exam.
Instead we have separate nodes for the basic types (e.g. `Int`, `Bool`, `Char`,
etc.). There's no fundamental difference between these two approaches, but we
wanted to flag it in case you saw `Lit` and got worried. It's the same approach
we use for the primitive operations, instead of a node for each primitive
operation, we have a node for _all_ primitive operations and use a symbol to
differentiate. Here we use a node for _all_ literals (`42`, `#f`, etc.) and
treat them accordingly.
