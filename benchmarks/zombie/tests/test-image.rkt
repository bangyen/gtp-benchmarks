#lang racket
(require rackunit
         rackunit/text-ui
         "../untyped/image.rkt")

;;;;????? what
; (struct-out image)
#|
(struct image (
 impl
))
|#


#|
(define (empty-scene w h)
  (when (or (negative? w) (negative? h))
    (error 'image "Arguments must be non-negative real numbers"))
  (image (cons w h)))
|#
(define empty-scene-tests
  (test-suite
   "empty-scene-tests"
   #:before (lambda () (display "START empty-scene-tests\n"))
   #:after  (lambda () (display "FINISH empty-scene-tests\n"))
   (test-case
    "not enough args"
    ;(check-exn-h (empty-scene))
    (check-exn exn:fail? (lambda () (empty-scene)))
    (check-exn exn:fail? (lambda () (empty-scene 1 2 23 5 6)))
    (check-exn exn:fail? (lambda () (empty-scene 10)))
    )
   (test-case
    "invalid w h"
    (check-exn exn:fail? (lambda () (empty-scene 'dog "cat")))
    (check-exn exn:fail? (lambda () (empty-scene "dog" 'cat)))
    (check-exn exn:fail? (lambda () (empty-scene 'dog 'cat)))
    (check-exn exn:fail? (lambda () (empty-scene 1 "poggers")))
    (check-exn exn:fail? (lambda () (empty-scene "poggers" 1)))
    (check-exn exn:fail? (lambda () (empty-scene 1 -1)))
    (check-exn exn:fail? (lambda () (empty-scene -1 -1)))
    (check-exn exn:fail? (lambda () (empty-scene -1 1)))
    )
   (test-suite
    "correct results"
    (test-case
     "(empty-scene 1 3)"
     (check-true (image? (empty-scene 1 3)))
     (check-true (cons? (image-impl (empty-scene 1 3))))
     (check-equal? 1 (car (image-impl (empty-scene 1 3))))
     (check-equal? 3 (cdr (image-impl (empty-scene 1 3))))
    )
    (test-case
     "(empty-scene 1111 999)"
     (check-true (image? (empty-scene 1111 999)))
     (check-true (cons? (image-impl (empty-scene 1111 999))))
     (check-equal? 1111 (car (image-impl (empty-scene 1111 999))))
     (check-equal? 999 (cdr (image-impl (empty-scene 1111 999))))
    ))
   ))

#|
(define (place-image i1 w h i2)
  (image (list i1 w h i2)))
|#

#|
(define (circle radius style color)
  (image (list radius style color)))
|# 


(run-tests empty-scene-tests)