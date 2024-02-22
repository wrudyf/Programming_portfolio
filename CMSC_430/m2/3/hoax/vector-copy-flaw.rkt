#lang racket
;; NOTE: New file.
;; TODO: Write a program below demonstrating the flaw.
;;the flaw is that we don't check to see if the original vector has e2 elements. so we might copy stuff beyond the size of the original vector

(vector_copy* (make-vector 3 10) 4)
