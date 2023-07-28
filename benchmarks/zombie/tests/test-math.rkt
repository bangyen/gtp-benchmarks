#lang racket

(require "../../../utilities/macro-for-tests.rkt"
         rackunit/text-ui
         ;; "../untyped/math.rkt"
         "math.rkt"
         )

;; tests for min
;; (define (min x y) (if (<= x y) x y))
(define min-test-suite
  (test-suite
   "Test for min function"
   ;; basic functionality with integers
   (check-eq? (min 5 6) 5 "min: x < y")
   (check-eq? (min 10 10) 10 "min: x == y")
   (check-eq? (min 30 -20) -20 "min: x > y")
   ;; basic functionality with floats
   (check-eq? (min 5.5 6.0) 5.5)
   (check-eq? (min -5.5 6) -5.5)
   (check-eq? (min -5 6.0) -5)
   (check-within (min 6 6.0) 6.0 0.01)
   (check-within (min 6.0 6) 6.0 0.01)
   ;; wrong inputs
   (check-exn exn:fail? (lambda () (min "string" 5)))
   (check-exn exn:fail? (lambda () (min 5 "string")))
   (check-exn exn:fail? (lambda () (min "string" "string")))
   ))

;; tests for max
;; (define (max x y) (if (>= x y) x y))
(define max-test-suite
  (test-suite
   "Test for max function"
   ; basic functionality for integers
   (check-eq? (max -35 15) 15 "max: x < y")
   (check-eq? (max 0 0) 0 "max: x == y")
   (check-eq? (max -50 -100) -50 "max: x > y")
   ; basic functionality with floats
   (check-eq? (max 5.5 6.0) 6.0)
   (check-eq? (max -5.5 6) 6)
   (check-eq? (max -5 6.0) 6.0)
   (check-within (max 6 6.0) 6.0 0.01)
   (check-within (max 6.0 6) 6.0 0.01)
   ;; wrong inputs
   (check-exn exn:fail? (lambda () (max "string" 5)))
   (check-exn exn:fail? (lambda () (max 5 "string")))
   (check-exn exn:fail? (lambda () (max "string" "string")))
   ))

;; tests for abs
;; (define (abs x) (if (>= x 0) x (- 0 x)))
(define abs-test-suite
  (test-suite
   "Test for abs function"
   ; basic functionality for integers
   (check-eq? (abs 5) 5 "abs: x > 0")
   (check-eq? (abs 0) 0 "abs: x == 0")
   (check-eq? (abs -1000) 1000 "abs: x < 0")
   ; basic functionality for floats
   (check-eq? (abs 5.5) 5.5)
   (check-eq? (abs 5.450450) 5.450450)
   (check-eq? (abs -5000) 5000)
   (check-eq? (abs 0.0) 0.0)
   (check-eq? (abs 0.5) 0.5)
   (check-equal? (abs -0.5) 0.5)
   ; wrong input
   (check-exn exn:fail? (lambda () (abs "")))
   ))

;; tests for sqr
;; (define (sqr x) (* x x))
(define sqr-test-suite
  (test-suite
   "Test for sqr function"
   ; basic integer and float functionality
   (check-eq? (sqr 5) 25 "sqr: x > 0")
   (check-equal? (sqr 5.5) 30.25 "sqr: x is not integer")
   (check-eq? (sqr -7) 49 "sqr: x < 0")
   (check-equal? (sqr -5.5) 30.25 "sqr: x is not integer and negative")
   ; wrong input
   (check-exn exn:fail? (lambda () (sqr "")))
   ))

;; tests for msqrt
;; (define (msqrt x) (assert (sqrt x) real?))
(define msqrt-test-suite
  (test-suite
   "Test for sqrt function"
   ; basic integer and float functionality
   (check-eq? (msqrt 4) 2 "msqrt: basic square root")
   (check-eq? (msqrt 9) 3 "msqrt: basic square root #2")
   (check-equal? (msqrt 8) (sqrt 8) "msqrt: non-natural square root")
   (check-equal? (msqrt 50) (sqrt 50) "msqrt: non-natural square root #2")
   (check-equal? (msqrt 5.5) (sqrt 5.5) "msqrt: non-natural square root #3")
   (check-exn exn:fail? (lambda () (msqrt -1)))
   ; wrong inputs
   (check-exn exn:fail? (lambda () (msqrt "50")))
   ))

;; run all test suites
(run-tests min-test-suite)
(run-tests max-test-suite)
(run-tests abs-test-suite)
(run-tests sqr-test-suite)
(run-tests msqrt-test-suite)
