#lang racket

(require rackunit
         rackunit/text-ui
         "../untyped/image.rkt")

(define test-suite-name
  (test-suite
   "name of test suite"
   #:before (lambda () (display "START\n"))
   #:after  (lambda () (display "FINISH\n"))
   (check-eq? 1 1)
   (check-eq? 2 2)
   (test-case
    "name of test case"
    (check < 1 2)
    (check > 3 2))
   ))

(run-tests test-suite-name)