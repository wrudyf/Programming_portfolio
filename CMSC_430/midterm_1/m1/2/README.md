# CMSC 430 Midterm 1, Part 2

## Instructions

You've been provided a file `instr.rkt` that contains a definition of
instructions `is`.  Update the definition to solve the following
problem.

Consider an alternative design for encoding the values in the Dupe
language:

- integer values are tagged with a 1 in the least significant bit
- boolean values are tagged with a 0 in the least significant bit

Suppose that the `rax` register holds the encoding of an integer value
`n1` and the `rbx` register holds the encoding of an integer value
`n2`.

Write a sequence of instructions that when executed will leave the
encoding of `(- n1 n2)` in the `rax` register.
