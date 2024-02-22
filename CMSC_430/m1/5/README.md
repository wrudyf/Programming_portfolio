# CMSC 430 Midterm 1, Part 5

## AST re-writes

You've been provided a modified implementation of Fraud: the compiler
implementation for begin has been removed. You must implement begin using other
features present in Fraud. Note that the Fraud language itself is unchanged;
any valid Fraud program from class should remain valid in this implementation.

`compile.rkt` has been modified to call a new function, `no-begin` that
you will define in `no-begin.rkt`. You should look at `compile.rkt` to see
how this function is used, but any modifications you make to `compile.rkt`
will be discarded, so all your work should be in `no-begin.rkt`.

`no-begin` will take in an AST (that can use the `Begin` node) but must convert
it to an AST that has zero `Begin` nodes, but would result in the same program
semantics. We have written some of the trivial cases to get you started, but
you'll have to do the rest.
