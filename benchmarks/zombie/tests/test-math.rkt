#lang racket

(require rackunit
         rackunit/text-ui
         "math.rkt"
         )

;; tests for min
(define min-test-suite
  (test-suite
   "Test for min function"
   (check-eq? (min 5 6) 5 "min: x < y")
   (check-eq? (min 10 10) 10 "min: x == y")
   (check-eq? (min 30 -20) -20 "min: x > y")
   ))

;; tests for max
(define max-test-suite
  (test-suite
   "Test for max function"
  (check-eq? (max -35 15) 15 "max: x < y")
  (check-eq? (max 0 0) 0 "max: x == y")
  (check-eq? (max -50 -100) -50 "max: x > y")))

;; tests for abs
(define abs-test-suite
  (test-suite
   "Test for abs function"
   (check-eq? (abs 5) 5 "abs: x > 0")
   (check-eq? (abs 0) 0 "abs: x == 0")
   (check-eq? (abs -1000) 1000 "abs: x < 0")
   ))

;; tests for sqr
(define sqr-test-suite
  (test-suite
   "Test for sqr function"
   (check-eq? (sqr 5) 25 "sqr: x > 0")
   (check-equal? (sqr 5.5) 30.25 "sqr: x is not integer")
   (check-eq? (sqr -7) 49 "sqr: x < 0")
   (check-equal? (sqr -5.5) 30.25 "sqr: x is not integer and negative")
   ))

;; tests for msqrt
(define msqrt-test-suite
  (test-suite
   "Test for sqrt function"
   (check-eq? (msqrt 4) 2 "msqrt: basic square root")
   (check-eq? (msqrt 9) 3 "msqrt: basic square root #2")
   (check-equal? (msqrt 8) (sqrt 8) "msqrt: non-natural square root")
   (check-equal? (msqrt 50) (sqrt 50) "msqrt: non-natural square root #2")
   (check-equal? (msqrt 5.5) (sqrt 5.5) "msqrt: non-natural square root #3")
   (check-exn exn:fail? (lambda () (msqrt -1)))))

;; run all test suites
(run-tests min-test-suite)
(run-tests max-test-suite)
(run-tests abs-test-suite)
(run-tests sqr-test-suite)
(run-tests msqrt-test-suite)