# CMSC 430 Midterm 1, Part 6

## Instructions

You've been provided an implementation of Extort as presented in
class.

Racket 9.0 has decided to finally break with tradition and change the
behavior of `if`.  No longer will it be possible to do conditional
evaluation using anything other than a bona fide boolean: `#t` or `#f`.

Your job is make Extort work like the forthcoming Racket 9.0.

Update `interp.rkt` and `compile.rkt` so that `if` works just like
before when the first subexpression evaluates to `#t` or `#f`, but
signals an error for any other value.

Note: none of the tests have been updated to reflect the new behavior,
so you will need to update the tests and may want to add your own as
well.
