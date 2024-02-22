# CMSC 430 Midterm 2


## Instructions

There are SIX parts to this midterm. Each part has its own directory with a
README and additional files. Read the README for the instructions on what to do.


## Communications

If you have questions about the exam, post a _private_ question on Piazza. We
will likely disable public posts during the exam time to make sure this doesn't
go wrong. Old (public) Piazza posts will still be visible.

Answers to common clarifying questions will be posted by course staff in an
updating FAQ on Piazza, similar to what we did for the last midterm. This is
post [@770](https://piazza.com/class/llwlfinnxr63c0/post/770).

You may not communicate with anyone outside of the course staff about the
midterm. Doing the midterm with other people will result in a referral to the
OSC.


## Submission

You should submit your work as a single zip file containing this directory.
Upload this zip to Gradescope.

To create a submission zip file, do `make submit.zip` from within the `m2/`
directory (the same directory where this README is located). Then upload the
resulting `submit.zip` file to Gradescope.

Unlike other assignments, Gradescope will only give feedback on the basic
well-formedness of your submission. This means it will check that the layout is
correct and all necessary files are present, and it will ensure you have
uploaded syntactically valid Racket code. You will not receive any feedback
about the correctness of your solutions.

If you fail these well-formedness tests, we will not be able to grade your
submission. Passing these tests only means your submission is well-formed; your
actual grade will be computed after the deadline. **IT IS YOUR RESPONSIBILITY TO
UPLOAD A WELL-FORMED SUBMISSION.**

To help you in cases where you can't figure out what's going wrong with your
submission, we've added the ability to disable grading for a specific question,
similar to what we provided for the Midterm 1 Playground. This will prevent the
autograder from running the well-formedness checks for that question or grading
it. If you do not disable questions that crash the autograder, we will be unable
to grade your exam. Look in the `info.rkt` file in this directory for
instructions on how to disable grading for specific questions.


## Notes

  * Each README has a Notes section with some relevant tips or reminders to help
    you out.

  * We left `TODO` comments in the places where you ought to write code.

  * We left `NOTE` comments in the places where we modified or added code.

  * We tried very hard to give sufficiently detailed explanations of the
    requirements for each part of the midterm. Although we will answer
    clarifying questions when you need help understanding what is asked of you,
    please do your best to look for the answers to your questions in the README
    files first.

  * You should not need to create any new files at any point, and we would
    prefer that you do not.

  * In some lectures, we do not use the `Lit` AST node, but you will find it in
    this exam. This AST node takes the place of the basic types (e.g., `Int`,
    `Bool`, `Char`, and so on). This is the same AST structure as we used on the
    last midterm.

  * We have provided updated forms of `asm-interp` and `asm-interp/io` that are
    used throughout the compiler code in these language distributions, but you
    can also use these functions for your own convenience. You can find them in
    `m2/utility/a86/interp.rkt`, which means you can `require` that file instead
    of `a86/interp` as you are used to. (Look in any `compile.rkt` file in a
    question's code to see the syntax if you're unsure about it.)


## To-do List

Using some shell magic, we can get a list of all the `TODO`s in the midterm
code. Something you might like to do is replace the `TODO` part with `DONE` when
you think you've finished a task and re-run the command to see what's left!

```shell
$ grep -r "TODO:" . | sort | sed -E 's/:[[:blank:]]*/\t/' | column -ts $'\t'
./1/pledge.rkt                                    ;; TODO: Put your name here to sign the pledge. To receive points for this
./2/hustle/compile-ops.rkt                        ;; TODO: Implement this!
./2/hustle/test/set-box-test-runner.rkt           ;; TODO: Add I/O tests here, if you like.
./2/hustle/test/set-box-test-runner.rkt           ;; TODO: Add non-I/O tests here, if you like.
./3/hoax/compile-ops.rkt                          ;; TODO: Find the flaw!
./3/hoax/test/vector-copy-test-runner.rkt         ;; TODO: Add I/O tests here, if you like.
./3/hoax/test/vector-copy-test-runner.rkt         ;; TODO: Add non-I/O tests here, if you like.
./3/hoax/vector-copy-flaw.rkt                     ;; TODO: Write a program below demonstrating the flaw.
./4/iniquity/compile.rkt                          ;; TODO: Complete the implementation according to the specifications.
./4/iniquity/test/default-define-test-runner.rkt  ;; TODO: Add I/O tests here, if you like.
./4/iniquity/test/default-define-test-runner.rkt  ;; TODO: Add non-I/O tests here, if you like.
./5/jig/compile-let-flaw.rkt                      ;; TODO: Replace the program below with one that demonstrates the issue.
./6/knock/compile.rkt                             ;; TODO: Implement the `app` pattern.
./6/knock/interp.rkt                              ;; TODO: Implement the `?` pattern.
./6/knock/test/new-patterns-test-runner.rkt       ;; TODO: Add I/O tests of ? here, if you like.
./6/knock/test/new-patterns-test-runner.rkt       ;; TODO: Add I/O tests of app here, if you like.
./6/knock/test/new-patterns-test-runner.rkt       ;; TODO: Add non-I/O tests of ? here, if you like.
./6/knock/test/new-patterns-test-runner.rkt       ;; TODO: Add non-I/O tests of app here, if you like.
```

NOTE: If you try to run the above command and get an error that `column` cannot
be found, you can just run the `grep -r "TODO:" .` part by itself and that
should work, though the results won't be as nicely formatted.
