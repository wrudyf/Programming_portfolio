#lang info

;; Don't change this.
(define submission-root #t)

;; If you upload your submission and Gradescope says that the tests timed out or
;; the autograder could not be run, you will either need to fix your submission
;; or else add the failing questions to this list. It's your job to figure out
;; what's going wrong --- Gradescope's feedback will likely not be able to help
;; with this.
;;
;; NOTE: The valid question identifiers are: P1, P2, P3.1, P3.2, P4, P5, P6. For
;; example, to skip questions P1 and P3.1, you would modify the below line like
;; this:
;;
;;     (define skip-questions '(P1 P3.1))
;;
;; Questions that are skipped will be marked as such in Gradescope.
(define skip-questions '())
