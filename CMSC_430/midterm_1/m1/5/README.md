# CMSC 430 Midterm 1, Part 5

## Instructions

You've been provided a modified implementation of Evildoer as
presented in class; in additional to the usual Evildoer features, it
includes an `and` form which works just like Racket's.

The `and` form can take any number of subexpressions and it evaluates
them from left to right.  Should any subexpression produce `#f`, the
`and` expression produces `#f` and subsequent subexpressions are not
evaluated.  If none of the subexpressions produce `#f`, then all of
the subexpressions are evaluated and the `and` expression produces the
value of the last expression (or `#t` if there is none).

It's important to note that `and` is "short-circuiting"; if any
subexpression produces `#f`, then subsequent subexpressions are not
evaluated.

Here are some examples:

```
> (and)
#t
> (and 8)
8
> (and #t 8 1)
1
> (and 8 #f (write-byte 97))
#f
```

The implementation you are given has already added `and` to the AST,
parser, and interpreter, and a stub has been added to the compiler
for handling `and`.  You simply have to complete the implementation
in the compiler.

Note: there are *some* tests for `and` included, but you should
consider adding more of your own.

Another note: the fact that this is Evildoer is not terribly relevant.
You could do this for Dupe and the solution would be the same.  The
reason Evildoer is used is so that we observe the short-circuiting as
shown in the last example.
