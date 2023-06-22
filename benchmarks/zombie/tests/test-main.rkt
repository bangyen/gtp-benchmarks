#lang racket
(require rackunit
         rackunit/text-ui
         "../untyped/main.rkt"
         )

(define replay-test-suite
  (test-suite
   "Test for replay function"
   (eq? 0 0)
   ))

(define realreal-test-suite
  (test-suite
   "Test for real-real-string-list? function"
   #:before (lambda () (display "START real-real-string-list?-tests\n"))
   #:after  (lambda () (display "FINISH real-real-string-list?-tests\n"))
   ;;correct list
   (check-true (real-real-string-list? '(3.14 2.718 "Hello")))
   (check-true (real-real-string-list? '(-1.2 2.7 "o")))
   (check-true (real-real-string-list? '(3.1 -2 "")))
   ;empty list
   (check-false (real-real-string-list? '()))
   ;list with wrong number of arguments
   (check-false (real-real-string-list? '(1)))
   (check-false (real-real-string-list? '(1 "1")))
   (check-false (real-real-string-list? '(0 "0" 0 "0")))
   ;;right length, elements wrong type
   (check-false (real-real-string-list? '("text" 1 "hi")))
   (check-false (real-real-string-list? '(1 2 3)))
   (check-false (real-real-string-list? '(1 "hello" 3)))
   ))

(define main-test-suite
  (test-suite
   "Test for main function"
   (eq? 0 0)
   ))

(run-tests realreal-test-suite)