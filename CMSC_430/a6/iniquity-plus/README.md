# Assignment 6: Arity Checking, Rest Arguments, Case Functions, and Apply

**Due: Monday, December 4, 11:59PM EST**

The goal of this assignment is to extend a compiler with arity checking for
function calls, to add new kinds of function parameter features, and to add the
[`apply`](http://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29)
form for applying a function to a list of arguments.

You are given a file `iniquity-plus.zip` on ELMS with a starter compiler similar
to the [Iniquity](https://www.cs.umd.edu/class/summer2023/cmsc430/Iniquity.html)
language we studied in class. You are tasked with:

  * implementing run-time arity checking for function calls
  * extending function definitions to include "rest argument" parameters for
    writing variable-arity functions,
  * extending function definitions to include
    [`case-lambda`](http://docs.racket-lang.org/reference/lambda.html#%28form._%28%28quote._~23~25kernel%29._case-lambda%29%29)-style
    multiple-arity functions,
  * extending the arity checking features to handle these new forms of function
    definitions, and
  * implementing the
    [`apply`](http://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29)
    mechanism for applying a function to the elements of a list as arguments.

Unlike previous assignments, you do not need to bring forward your past features
to this language; there is no need to implement `cond`, `case`, etc.

Be sure to read the entire problem description before starting. There are a
number of [Suggestions](#suggestions) on how to approach the assignment near the
end.


## Checking arity

In Iniquity, we implemented a language with function definitions and calls. We
noted that bad things can happen when a function is called with the incorrect
number of arguments. While it's possible to statically check this property of
Iniquity programs, it's not possible in more expressive language and arity
checking must be done at run-time. You are tasked with implementing such a
run-time arity checking mechanism.

Here is the basic idea. You need to add a run-time checking mechanism that will
cause the following program to signal an error:

```scheme
(define (f x y) (+ x y))
(f 1)
```

The function call knows how many arguments are given and the function definition
knows how many arguments are expected. The generated code should check that
these two quantities match when the function is called.

A simple way to do this is to pick a designated register that will be used for
communicating arity information. The caller should set the register to the
number of arguments before jumping to the function. The function should check
this number against the expected number and signal an error when they don't
match.

You should modify `compile-app` and `compile-define` to implement this part of
the assignment.


## Rest arguments

Many languages, including JavaScript, C, and Racket, provide facilities for
defining functions that take a "rest argument" which allows the function to be
called with more arguments than expected and these additional arguments will be
bound to a single value that collects all of these arguments. In Iniquity, as in
Racket, the obvious way of collecting these arguments into a single value is to
use a list.

Here are some examples:

  * `(define (f . xs) ...)`

    This function takes any number of arguments and binds `xs` to a list
    containing all of them.

  * `(define (f x . xs) ...)`

    This function takes at least one argument and binds `x` to the first
    argument and `xs` to a list containing the rest. It's an error to call this
    function with zero arguments.

  * `(define f x y z . xs) ...)`

    This function takes at least three arguments and binds `x`, `y`, and `z` to
    the first three arguments and `xs` to a list containing the rest. It's an
    error to call this function with 0, 1, or 2 arguments.

Here are some examples in Racket to get a sense of the behavior:

```scheme
> (define (f . xs) (list xs))
> (f)
'(())
> (f 1)
'((1))
> (f 1 2)
'((1 2))
> (f 1 2 3)
'((1 2 3))
> (f 1 2 3 4)
'((1 2 3 4))
> (define (f x . xs) (list x xs))
> (f)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: at least 1
  given: 0
> (f 1)
'(1 ())
> (f 1 2)
'(1 (2))
> (f 1 2 3)
'(1 (2 3))
> (f 1 2 3 4)
'(1 (2 3 4))
> (define (f x y z . xs) (list x y z xs))
> (f)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: at least 3
  given: 0
> (f 1)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: at least 3
  given: 1
> (f 1 2)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: at least 3
  given: 2
> (f 1 2 3)
'(1 2 3 ())
> (f 1 2 4)
'(1 2 4 ())
```

The code generated for a function call should not change---other than what you
did for [Checking arity](#checking-arity): it should pass all of the arguments
on the stack along with information about the number of arguments.

The compilation of function definitions that use a rest argument should generate
code that checks that the given number of arguments is acceptable and should
generate code to pop all "extra" arguments off the stack and construct a list
which is then bound to the rest parameter.

It is worth remembering that arguments are pushed on the stack in such a way
that the last argument is the element most recently pushed on the stack. This
has the benefit of making it easy to pop off the extra arguments and to
construct a list with the elements in the proper order.

HINT: The function definition knows the number of "required" arguments, i.e.,
the minimum number of arguments the function can be called with---call this
_m_---and the caller communicates how many actual arguments have been
supplied---call this _n_. The compiler needs to generate a loop that pops _n-m_
times, constructing a list with popped elements, and then finally pushes this
list in order to bind it to the rest parameter.


## Arity dispatch

Some languages, such as Java, Haskell, and Racket, make it possible to overload
a single function name with multiple definitions where the dispatch between
these different definitions is performed based on the number (or kind) of
arguments given at a function call.

In Racket, this is accomplished with the
[`case-lambda`](http://docs.racket-lang.org/reference/lambda.html#%28form._%28%28quote._~23~25kernel%29._case-lambda%29%29)
form for constructing multiple-arity functions.

Here is an example:

```scheme
> (define f
    (case-lambda
      [(x) "got one!"]
      [(p q) "got two!"]))
> (f #t)
"got one!"
> (f #t #f)
"got two!"
> (f #t #f 0)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  given: 3
```

This function can accept either one or two arguments. If given one argument, it
evaluates the right-hand side of the first clause with `x` bound to that
argument. If given two arguments, it evaluates the right-hand side of the second
clause with `p` and `q` bound to the arguments. If given any other number of
arguments, it signals an error.

A `case-lambda` form can have any number of clauses (including zero!) and the
first clause for which the number of arguments is acceptable is taken when the
function is called.

Note that `case-lambda` can be combined with rest arguments too. A clause that
accepts any number of arguments is written by simply listing a parameter name
(no parentheses). A clause that accepts some non-zero minimum number of
parameters is written with a dotted parameter list.

For example:

```scheme
> (define f
    (case-lambda
      [(x y z . r) (length r)]
      [(x) "just one!"]))
> (f 1 2 3 4 5 6)
3
> (f #t)
"just one!"
> (f)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  given: 0
> (f 1 2)
f: arity mismatch;
 the expected number of arguments does not match the given
number
  given: 2
```

This function takes three or more arguments or one argument. Any other number of
arguments (i.e., zero or two) results in an error.

```scheme
> (define f
    (case-lambda
      [(x y z) "three!"]
      [xs (length xs)]))
> (f)
0
> (f 1 2)
2
> (f 1 2 3)
"three!"
> (f 1 2 3 4 5 6)
6
```

This function takes any number of arguments, but when given three, it produces
`"three!"`; in all other cases it produces the number of arguments.


## Apply

Apply is the yin to the yang of rest arguments (or maybe the other way around).
Whereas a rest argument lets a function take arbitrarily more arguments and
packages them up as a list,
[`apply`](http://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29)
will apply a function to a list as though the elements of the list were given as
arguments.

```scheme
> (define (f x y) (+ x y))
> (apply f (list 1 2))
3
> (define (flatten ls)
    (apply append ls))
> (flatten (list (list 1 2) (list 3 4 5) (list 6)))
'(1 2 3 4 5 6)
> (define (sum ls)
    (apply + ls))
> (sum (list 5 6 7 8))
26
```

Here you can see `apply` taking two things: a function and a single argument
which is a list. It is calling the function with the elements of the list as the
arguments.

It turns out, `apply` can also take other arguments in addition to the list and
pass them along to the function.

```scheme
> (define (f x y) (+ x y))
> (apply f 1 (list 2))
3
> (apply list 1 2 3 4 (list 5 6 7))
'(1 2 3 4 5 6 7)
```

Note that if the function expects a certain number of arguments and the list has
a different number of elements, it results in an arity error:

```scheme
> (define (f x y) (+ x y))
> (apply f (list 1 2 3))
f: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: 2
  given: 3
```

A new form of expression has been added to the `Expr` AST type:

```scheme
; type Expr = ...
;           | (Apply Id [Listof Expr] Expr)

```

The parser has been updated to handle concrete syntax of the form:

```scheme
(apply f e0 ... en)
```

Example:

```scheme
> (parse-e '(apply f x y zs))
'#s(Apply f (#s(Var x) #s(Var y)) #s(Var zs))
```

Note that the AST for an `apply` expression has the function name, an
arbitrarily long list of arguments, plus a distinguished last argument that
should produce a list. (It is an error if this expression produces anything
other than a list.)

While it's allowable to have only the function and the list argument, it's a
syntax error to leave off a list argument altogether:

```scheme
> (parse-e '(apply f xs))
'#s(Apply f () #s(Var xs))
> (parse-e '(apply f))
parse apply error
```

The interpreter also handles `apply` expressions:

```scheme
> (interp (parse '[ (define (f x y) (cons y x))
                    (apply f (cons 1 (cons 2 '())))]))
'(2 . 1)
```

Together with rest arguments, `apply` makes it possible to write many functions
you may like to use:

```scheme
> (interp
    (parse
      '[; an append that works on any number of lists
        (define (append . xss)
          (if (empty? xss)
              '()
              (if (empty? (car xss))
                  (apply append (cdr xss))
                  (cons (car (car xss))
                        (apply append (cdr (car xss)) (cdr xss))))))
         ; the list function!
         (define (list . xs) xs)

         (append (list 1 2 3) (list 4) (list 5 6 7))]))
'(1 2 3 4 5 6 7)
```

In `compile.rkt`, the `compile-e` has an added case for `Apply` AST nodes and
calls `compile-apply`, which is stubbed out for you. You will need to implement
`apply` there.

Here is the idea for `apply`: it is doing something similar to a function call,
so it needs to make a label for the return point and push that on the stack. It
then needs to execute all of the given arguments, pushing them on the stack
(again just like a regular function call). Then it needs to execute the
distinguished list argument and generate code that will traverse the list at
run-time, pushing elements on to the stack until reaching the end of the list.
At this point, all of the arguments (both those given explicitly and those in
the list) are on the stack. Jump to the function.


## Representing the syntax of function definitions

The Iniquity language has a single function definition form: `(define (f x...)
e)`, which is represented with the following AST type:

```scheme
; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)
```

Because there are three different forms of function definition in Iniquity+, we
use the following AST representation:

```scheme
; type Defn = (Defn Id Fun)
(struct Defn (f fun) #:prefab)

; type Fun = (FunPlain [Listof Id] Expr)
;          | (FunRest [Listof Id] Id Expr)
;          | (FunCase [Listof FunCaseClause])
; type FunCaseClause = (FunPlain [Listof Id] Expr)
;                    | (FunRest [Listof Id] Id Expr)
(struct FunPlain (xs e)   #:prefab)
(struct FunRest  (xs x e) #:prefab)
(struct FunCase  (cs)     #:prefab)
```

What used to be represented as `(Defn f xs e)` is now represented as `(Defn f
(FunPlain xs e))`.

The parser already works for these new forms of function definitions. Here are
some examples of how function definitions are parsed, but you are encouraged to
try out more to get a better sense:

```scheme
> (parse-define '(define (f x) x))
'#s(Defn f #s(FunPlain (x) #s(Var x)))
> (parse-define '(define (f . xs) xs))
'#s(Defn f #s(FunRest () xs #s(Var xs)))
> (parse-define '(define (f x y z . q) q))
'#s(Defn f #s(FunRest (x y z) q #s(Var q)))
> (parse-define
    '(define f
       (case-lambda
         [(x y) 2]
         [(z) 1]
         [(a b c . d) "3+"]
         [q "other"])))
'#s(Defn
    f
    #s(FunCase
       (#s(FunPlain (x y) #s(Int 2))
        #s(FunPlain (z) #s(Int 1))
        #s(FunRest (a b c) d #s(Str "3+"))
        #s(FunRest () q #s(Str "other")))))

```


## Starter code

The compiler code given to you is just an implementation of Iniquity, but
updated to parse the new forms of function definitions and re-organized slightly
to match the new AST representation.

The interpreter code given to you works on the full Iniquity+ language, so you
do not need to update `interp.rkt` and can use the interpreter to guide your
implementation of the compiler.

```scheme
> (interp
    (parse '[(define (f x) x)
             (f 1)]))
1
> (interp
    (parse '[(define (f . x) x)
             (f 1)]))
'(1)
> (interp
    (parse '[(define (f . x) x)
             (f)]))
'()
> (interp
    (parse '[(define (f . x) x)
             (f 1 2 3 4 5)]))
'(1 2 3 4 5)
> (interp
    (parse '[(define f
               (case-lambda
                 [(x y) 2]
                 [(z) 1]
                 [(a b c . d) "3+"]
                 [q "other"]))
              (cons (f 7)
                    (cons (f 3 4)
                          (cons (f)
                                (cons (f 7 8 9 10 11)
                                      '()))))]))
'(1 2 "other" "3+")
```

Thus, you should only need to modify `compile.rkt`.

A small number of test cases are given as usual.


## Suggestions

This is a tricky assignment. The amount of code you have to write is pretty
small, however you may spend a long time slogging through the assignment if your
approach is to hack first, think later.

Here are some suggestions for how to approach the assignment. Make sure you get
each of the pieces working before moving on.

  * Start with [Checking arity](#checking-arity); this should be pretty easy.
    Make sure it works for plain function definitions.

  * Move on to [Rest arguments](#rest-arguments). You could start by emitting
    code that checks that the arguments are acceptable, popping the appropriate
    number of arguments off (and ignoring the elements), then pushing the empty
    list. This will work like a rest arg in that it should accept any number of
    arguments beyond the required minimum, but the rest argument will always be
    bound to the empty list. Once working, try to modify the code to build a
    list as it pops arguments. Test that it works.

  * Next you could either tackle `case-lambda` or `apply`.

      * For [Arity dispatch](#arity-dispatch), remember that you have a compiler
        for plain and rest argument functions at this point. That should come in
        handy. Think of `case-lambda` as generating a set of function
        definitions (with generated names), and then the main work of
        `case-lambda` is determining which of the generated functions to call,
        given the specific number of arguments passed in by the caller. When you
        find the function that fits, jump to it. You might start by only
        handling plain function clauses in `case-lambda` before moving on to
        handling rest argument functions, too.

      * For [Apply](#apply), at first don't worry about arity checking and
        consider the case where there are no explicit arguments given, i.e.,
        focus on `(apply f e)`. Once you have that working, consider the more
        general case of `(apply f e0 ... e)`. Then figure out how to add in the
        arity checking part. Finally, make sure you're detecting error cases
        such as when `e` is not a proper list.


## Submitting

Submit a `.zip` file containing your work to Gradescope. Use `make submit.zip`
from within the `iniquity-plus` directory to create a `.zip` file with the
proper structure.
