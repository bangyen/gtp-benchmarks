#lang racket
(require rackunit
         rackunit/text-ui
         
         ;"image.rkt"
         "../untyped/image.rkt"
         )

(provide place-image-check
         test-image-equal)


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
   (test-suite
    "not enough args"
    ;(check-exn-h (empty-scene))
    (check-exn exn:fail? (lambda () (empty-scene)))
    (check-exn exn:fail? (lambda () (empty-scene 1 2 23 5 6)))
    (check-exn exn:fail? (lambda () (empty-scene 10)))
    )
   (test-suite
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
    (test-suite
     "(empty-scene 1 3)"
     (check-true (image? (empty-scene 1 3)))
     (check-true (cons? (image-impl (empty-scene 1 3))))
     (check-equal? 1 (car (image-impl (empty-scene 1 3))))
     (check-equal? 3 (cdr (image-impl (empty-scene 1 3))))
    )
    (test-suite
     "(empty-scene 1111 999)"
     (check-true (image? (empty-scene 1111 999)))
     (check-true (cons? (image-impl (empty-scene 1111 999))))
     (check-equal? 1111 (car (image-impl (empty-scene 1111 999))))
     (check-equal? 999 (cdr (image-impl (empty-scene 1111 999))))
    ))
   ))

;; (place-image-check fxn i1 i2 i3 i4 msg)
;      - fxn is a lambda call to place-image
;      - i1, i2, i3, i4 are the args passed to the place-image call in order
;      - i1 and i4 are images
;      - i2 and i3 are Real numbers
; returns a test suite that checks if the result of place-image is correct.
(define (place-image-check fxn i1 i2 i3 i4 msg)
  (define result (fxn))
  (test-suite
   msg
   (check-true (image? result))
   (check-true (list? (image-impl result)))
   (check-equal? (length (image-impl result)) 4)
   ; first: image
   (check-true (image? (first (image-impl result))))
   (test-image-equal i1 (first (image-impl result)))
   ; second: real
   (check-true (real? (second (image-impl result))))
   (check-equal? i2 (second (image-impl result)))
   ; third: real
   (check-true (real? (third (image-impl result))))
   (check-equal? i3 (third (image-impl result)))
   ; fourth: iamge
   (check-true (image? (fourth (image-impl result))))
   (test-image-equal i4 (fourth (image-impl result)))
  ))
(define place-image-tests
  (test-suite
   "place-image-tests"
   #:before (lambda () (display "START place-image-tests\n"))
   #:after  (lambda () (display "FINISH place-image-tests\n"))
   (test-suite
    "not enough args"
    (check-exn exn:fail? (lambda () (place-image 1 2)))
    (check-exn exn:fail? (lambda () (place-image)))
    (check-exn exn:fail? (lambda () (place-image 1 2 3 4 5)))
    (check-exn exn:fail? (lambda () (place-image 1 2 3 4 5 6 7 8 9 10)))
    )
   (test-suite
    "invalid inputs"
    (check-exn exn:fail? (lambda () (place-image-check (place-image 5 6 7 (empty-scene 12 10))
                                                       5 6 7 (empty-scene 12 10)
                                                       "i1 not image")))
    (check-exn exn:fail? (lambda () (place-image-check (place-image (circle 10 "s" "p") 6 7 10)
                                                       (circle 10 "s" "p") 6 7 10
                                                       "i4 not image")))
    (check-exn exn:fail? (lambda () (place-image-check (place-image (circle 10 "s" "p") 6 7 'dog)
                                                       (circle 10 "s" "p") 6 7 'dog
                                                       "i4 not image")))
    (check-exn exn:fail? (lambda () (place-image-check (place-image (circle 10 "s" "p") 6 "monkey" (empty-scene 50 50))
                                                       (circle 10 "s" "p") 6 "monkey" (empty-scene 50 50)
                                                       "i3 not real")))
    (check-exn exn:fail? (lambda () (place-image-check (place-image (circle 10 "s" "p") 'ghost 8 (empty-scene 50 50))
                                                       (circle 10 "s" "p") 'ghost 8 (empty-scene 50 50)
                                                       "i2 not real")))
    )
   (test-suite
    "expected results"
    (place-image-check (lambda () (place-image (circle 5 "solid" "pink") 300 500 (empty-scene 300 500)))
                       (circle 5 "solid" "pink") 300 500 (empty-scene 300 500)
                       "place-image 1")
    (place-image-check (lambda () (place-image (circle -10 "solid" "pink") -1 0 (empty-scene 300 500)))
                       (circle -10 "solid" "pink") -1 0 (empty-scene 300 500)
                       "place-image neg nums")
    (place-image-check (lambda () (place-image (empty-scene 50 50) -4 -100 (empty-scene 20 30)))
                       (empty-scene 50 50) -4 -100 (empty-scene 20 30)
                       "place-image two empty scenes, both negative coords")
   )
   (test-suite
    "test-image-equal"
    (test-image-equal (circle 4 "solid" "red")
                      (circle 4 "solid" "red"))
    (test-image-equal (empty-scene 4 10)
                      (empty-scene 4 10))
    (test-image-equal (image '(20 "transparent" 30))
                      (image '(20 "transparent" 30)))
    (test-image-equal (image '((circle 4 "solid" "pink")
                               100
                               150
                               (empty-scene 100 150)))
                      (image '((circle 4 "solid" "pink")
                               100
                               150
                               (empty-scene 100 150))))
    (test-image-equal (image '((circle 4 "solid" "pink")
                               100
                               150
                               (image '((circle 10 "solid" "blue")
                                        3 6
                                        (empty-scene 100 150)))))
                      (image '((circle 4 "solid" "pink")
                               100
                               150
                               (image '((circle 10 "solid" "blue")
                                        3 6
                                        (empty-scene 100 150)))))) 
   )))

#|
(define (circle radius style color)
  (image (list radius style color)))
|#
(define (check-circle fxn i1 i2 i3 msg)
  (define result (fxn))
  (test-suite
   msg
   (check-true (image? result))
   (check-true (list? (image-impl result)))
   (check-equal? (length (image-impl result)) 3)
   (check-equal? (first (image-impl result)) i1)
   (check-equal? (second (image-impl result)) i2)
   (check-equal? (third (image-impl result)) i3)
   ))
(define circle-tests
  (test-suite
   "circle-tests"
   #:before (lambda () (display "START circle-tests\n"))
   #:after  (lambda () (display "FINISH circle-tests\n"))
   (test-suite
    "not enough args"
    (check-exn exn:fail? (lambda () (circle 1 2)))
    (check-exn exn:fail? (lambda () (circle)))
    (check-exn exn:fail? (lambda () (circle 1 2 3 4)))
    (check-exn exn:fail? (lambda () (circle 1 2 3 4 5 6 7 8 9 10)))
    )
   (test-suite
    "expected results"
    (check-circle (lambda () (circle 50 "bold" 'red))
                  50 "bold" 'red
                  "typical circle")
    (check-circle (lambda () (circle -1 0 1))
                  -1 0 1
                  "-1 0 1 circle")
    (check-circle (lambda () (circle '(90 10 11) -1000 "boo"))
                  '(90 10 11) -1000 "boo"
                  "funky input types")
    )
))

;; (test-image-equal img1 img2)
;; checks equality of images
;; image-impl is a cons (a list is a cons)
;; returns a test suite or fail
(define (test-image-equal img1 img2)
  (unless (image? img1)
    (fail "img1 not an image"))
  (unless (image? img1)
    (fail "img2 not an image"))
  (cond
    ; both lists
    [(and (list? (image-impl img1))
          (list? (image-impl img2)))
     (cond
       ; equal length
       [(= (length (image-impl img1))
           (length (image-impl img2)))
        (define l (length (image-impl img1)))
        (test-suite
         "both lists of equal length"
         (for ([a (in-list (image-impl img1))]
               [b (in-list (image-impl img2))])
           (if (and (image? a) (image? b))
               (test-image-equal a b)
               (if (and (not (image? a))
                        (not (image? b)))
                   (check-equal? a b)
                   (fail "not both images")))))]
       ; unequal length
       [else (fail "lists are not equal length")]
       )]
    ; both cons and not lists
    [(and (cons? (image-impl img1))
          (cons? (image-impl img2))
          (not (list? (image-impl img1)))
          (not (list? (image-impl img2))))   
     (test-suite
      "both cons"
      (check-equal? (car (image-impl img1))
                    (car (image-impl img2)))
      (check-equal? (cdr (image-impl img1))
                    (cdr (image-impl img2)))
      )]
    ; not both cons or both lists
    [else (fail "image-impl not both cons or not both lists")]
  ))


(run-tests empty-scene-tests)
(run-tests place-image-tests)
(run-tests circle-tests)