#lang racket
(require rackunit
         rackunit/text-ui
         "../untyped/main.rkt"
         "../untyped/zombie.rkt"
         (submod "../untyped/zombie.rkt" test)
         (submod "../untyped/main.rkt" test)
         )

(define replay-test-suite
  (test-suite
   "Test for replay function"
   #:before (lambda () (display "START replay-tests\n"))
   #:after  (lambda () (display "FINISH replay-tests\n"))
   ;wrong number of arguments
   (check-exn exn:fail? (lambda () (replay 1 2 3)))
   (check-exn exn:fail? (lambda () (replay 1 '() "text")))
   ;empty list, return void
   (check-true (void? (replay 1 '())))
   (check-true (void? (replay '() '())))
   (check-true (void? (replay "text" '())))
   (check-true (void? (replay w0 '())))
   ;not list, exception
   (check-exn exn:fail? (lambda () (replay 1 2)))
   (check-exn exn:fail? (lambda () (replay 1 "text")))
   (check-exn exn:fail? (lambda () (replay '() 2)))
   (check-exn exn:fail? (lambda () (replay '() 2)))
   ;correct input
   (check-true (void? (replay w0 '((on-mouse 367 212 "enter") '(on-mouse 367 211 "move")))))
   (check-true (void? (replay w0 '((on-tick) (on-mouse 367 212 "enter")))))
   (check-true (void? (replay w0 '((on-mouse 367 212 "enter") '() (on-tick)))))
   (check-true (void? (replay w0 '((on-mouse 367 212 "enter") (on-tick) '() (on-tick)))))
   (check-true (void? (replay w0 '((on-mouse 367 212 "enter") (random) (unknown) (on-tick)))))
   ;;wrong element in hist
   (check-exn exn:fail? (lambda () (replay w0 '(1 (on-mouse 367 211 "move")))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse 367 211 "move") 1))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse 367 211 "move") ()))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse 367 211 "move") "text"))))
   ;;wrong element in on-mouse (not realrealstring)
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse 367)))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse "text")))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse 1 2 3 4)))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse "text" "text" 1)))))
   (check-exn exn:fail? (lambda () (replay w0 '((on-mouse 1 "text" 1)))))
   ;not world
   (check-exn exn:fail? (lambda () (replay 0 '((on-mouse 367 212 "enter") '(on-mouse 367 211 "move")))))
   (check-exn exn:fail? (lambda () (replay 1 '((on-mouse 367 212 "enter") '(on-mouse 367 211 "move")))))
   ))

(define realrealstring-test-suite
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
   ;wrong input type
   (check-exn exn:fail? (lambda () (main 1)))
   (check-exn exn:fail? (lambda () (main "text")))
   ;wrong number of arguments
   (check-exn exn:fail? (lambda () (main 2 "text")))
   (check-exn exn:fail? (lambda () (main '() 1)))
   (check-exn exn:fail? (lambda () (replay "text" '() 3)))
   ;correct input
   (check-true (void? (main '())))
   (check-true (void? (main '('(on-tick) '(on-mounse 1 1 "enter")))))
   ))

(run-tests realrealstring-test-suite)
(run-tests replay-test-suite)
(run-tests main-test-suite)