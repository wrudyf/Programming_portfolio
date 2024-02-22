#lang racket
;;this works, it produced a contract violation in racket interpreter
;;because zero? expects an int, but gets a bool
;;however, when we compile zero?, we just compare rax to value 0, and it will return a value
(zero? #f)
