#lang racket
;;this is a problem, if we use interp when we compile and run this program
;;interp will return the result of our program as 5 every time, even if we type #f
;;that is because it interprets read-byte before it actually reads any info and then just says
;;since it's not false, it must be truthy and so the program ends up returning 5
(if (read-byte) 5 10)
