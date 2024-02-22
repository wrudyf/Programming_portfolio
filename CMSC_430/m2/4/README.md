# CMSC 430 Midterm 2, Part 4


## Instructions

You've been provided a modified implementation of the Iniquity language that was
presented in class.

We've extended the functionality of Iniquity to include more expressive `define`
forms for function definitions. Specifically, you can now have an _optional_
final parameter with a default argument to be used when needed. Here is an
example of using this feature in the Racket REPL (our modification uses the same
syntax):

```scheme
> (define (sum-or-add2 a [b 2])
    (+ a b))
> (sum-or-add2 3)
5
> (sum-or-add2 3 4)
7
```

We have fully implemented the parser and partially implemented the interpreter
and compiler for you. Your job is to finish the implementation **in the
compiler**.

The way we have implemented the feature in your starter code requires that the
function is always applied with all _n_ arguments. If it is applied with any
other number of arguments, an error is raised. What you need to do is modify the
code so that when _n-1_ arguments are given, the default argument is used for
the final parameter.


## Notes

  * Our implementation of default arguments only needs to work for the final
    parameter in a function definition. Do not try to extend the functionality.

  * **You do not have to update the interpreter.** The interpreter is only
    implemented to mirror the initial compiler for your convenience, but you can
    completely ignore it if you like. If you want to run the compiler tests and
    not the interpreter tests, you can do `raco test test/compile.rkt`.

  * We have added some new tests in `test/default-define-test-runner.rkt`. If
    you want to write new tests of your own, you are welcome to do so in this
    file.


## Parsing

The `parse` function used in this version of Iniquity may be slightly different
from what you're used to, depending on what you've seen in class. Specifically,
it takes all the top-level forms of the program as separate arguments. For
example, to `parse` a program with two function definitions and then a main
program expression, you might do:

```scheme
> (require "parse.rkt")
> (parse '(define (len xs)
            (if (empty? xs)
                0
                (add1 (len (cdr xs)))))
         '(define (len-or-n xs [n 0])
            (let ([l (len xs)])
              (if (zero? l)
                  n
                  l)))
         '(len-or-n (cons 1 '()) 2))
'#s(Prog
    (#s(PlainDefn
        len
        (xs)
        #s(If
           #s(Prim1 empty? #s(Var xs))
           #s(Lit 0)
           #s(Prim1 add1 #s(App len (#s(Prim1 cdr #s(Var xs)))))))
     #s(DefaultDefn
        len-or-n
        (xs)
        n
        #s(Lit 0)
        #s(Let
           l
           #s(App len (#s(Var xs)))
           #s(If #s(Prim1 zero? #s(Var l)) #s(Var n) #s(Var l)))))
    #s(App len-or-n (#s(Prim2 cons #s(Lit 1) #s(Empty)) #s(Lit 2))))
```

The starter code handles this just fine on its own, but if you do your own
testing in the REPL you'll want to know about this.
