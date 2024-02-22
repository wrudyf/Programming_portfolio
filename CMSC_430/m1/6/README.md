# CMSC 430 Midterm 1, Part 6

## Instructions

You've been provided an implementation of Extort as presented in
class.

After having programmed in C again, we realized that loops are great
and all other forms of iteration are mediocre. As such we're going
to add loops to Extort using the following new expression form:

- `(repeat e1 e2)` evaluates as follows:

   * `e1` is evaluated to an integer 'i'.
     - if 'i' <= 0, then the program results in `'err`
     - if 'i' is not an integer at all, the program results in `'err`

   * `e2` is then evaluated 'i' times

   * the overall value of `(repeat e1 e2)` is the value of `e2`
     when evaluated the 'i'th time.
   
The AST type definition, parser, and interpreter have been updated to
implement these new forms, and a stub has been added to the
compiler. You must finish the implementation in the compiler.

Update `interp.rkt` and `compile.rkt` so that `repeat` works just like
above.

Note: `repeat` _does not_ exist in this form in Racket. This is new,
we're breaking new ground. 

Note: none of the tests have been updated to reflect the new behavior,
so you will need to update the tests and may want to add your own as
well.
