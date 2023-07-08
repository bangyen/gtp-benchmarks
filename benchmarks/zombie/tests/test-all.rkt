#lang racket
(require rackunit
         rackunit/text-ui
         "image.rkt"
         ;"../untyped/image.rkt"
         "math.rkt"
         ;"../untyped/math.rkt"
         (submod "zombie.rkt" test)
         ;(submod "../untyped/zombie.rkt" test)
         (submod "main.rkt" test)
         ;(submod "../untyped/main.rkt" test)
         )

;; ----------------------------------------------------
;; IMAGE

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
    #|(test-image-equal (image '(20 "transparent" 30))
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
                                        (empty-scene 100 150))))))|# 
   )))


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
    #|(check-circle (lambda () (circle 50 "bold" 'red))
                  50 "bold" 'red
                  "typical circle")
    (check-circle (lambda () (circle -1 0 1))
                  -1 0 1
                  "-1 0 1 circle")
    (check-circle (lambda () (circle '(90 10 11) -1000 "boo"))
                  '(90 10 11) -1000 "boo"
                  "funky input types")|#
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

;; ------------------------------------------------------
;; MATH

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

;; -----------------------------------------------------------
;; NEW-POSN

;; UTILITIES

; (posn-equal? posn-1 posn-2)
;      - posn-1 and posn-2 must be a posn returned by (new-posn ...)
; checks if posn-1 and posn-2 have matching x and y values.
; returns a boolean.
(define (posn-equal? posn-1 posn-2)
  (and (= ((posn-x posn-1))
          ((posn-x posn-2)))
       (= ((posn-y posn-1))
          ((posn-y posn-2)))))

(define (check-posn-dist posn-1 posn-2)
  (check-within ((posn-dist posn-1) posn-2)
                (sqrt (+ (* (- ((posn-x posn-1))
                               ((posn-x posn-2)))
                            (- ((posn-x posn-1))
                               ((posn-x posn-2))))
                         (* (- ((posn-y posn-1))
                               ((posn-y posn-2)))
                            (- ((posn-y posn-1))
                               ((posn-y posn-2))))))
                0.001))

;; test check-posn-distance
(check-posn-dist (new-posn 0 0) (new-posn 50 50))

; (algo-move-toward from-posn to-posn speed)
; from-posn and to-posn are posns returned by (new-posn ...)
; runs a copy of the posn-move-toward/speed algo with from-posn moving towards to-posn.
; returns a posn created by (new-posn ...) with the new location of from-posn
(define (algo-move-toward from-posn to-posn speed)
  ; x1 y1 is our current location
  (define x1 ((posn-x from-posn)))
  (define y1 ((posn-y from-posn)))
  ; x2 y2 is where we move towards
  (define x2 ((posn-x to-posn)))
  (define y2 ((posn-y to-posn)))
  ; xmove and ymove are the amounts we move in x and y directions
  (define xmove (- x2 x1))
  (define ymove (- y2 y1))
  ; distance to move in either x or y direction
  (define dist (min speed
                    (max (abs xmove)
                         (abs ymove))))
  ; we move dist in the y direction if xmove < ymove, otherwise move in x direction
  (if (< (abs xmove) (abs ymove))
      (new-posn x1
                (if (positive? ymove)
                    (+ y1 dist)
                    (- y1 dist)))
      (new-posn (if (positive? xmove)
                    (+ x1 dist)
                    (- x1 dist))
                y1))
  )

; (test-posn posn-fxn x y msg)
;     - posn-fxn is a lambda call to (new-posn ...)
;     - x and y are the vals passed to (new-posn ...), in order
; returns a test suite that checks if posn-fxn gives a valid posn.
; example usage: (test-posn (lambda () (new-posn 0 1)) 0 1)
(define (test-posn posn-fxn x y msg)
  (define result (posn-fxn))
  (define test-posn-1 (new-posn 0 0))
  
  (define test-move-toward-1 ((posn-move-toward/speed result)
                              test-posn-1 1))
  (define test-move-toward-2 ((posn-move-toward/speed result)
                              test-posn-1 (+ (abs x) (abs y))))

  (test-suite
   msg
   ;; posn-x
   (check-eq? ((posn-x result)) x)
   ;; posn-y
   (check-eq? ((posn-y result)) y)
   ;; posn-posn
   (check-eq? ((posn-x ((posn-posn result)))) x)
   (check-eq? ((posn-y ((posn-posn result)))) y)
   ;; move-toward/speed
   (check-eq? ((posn-x test-move-toward-1))
              (if (> (abs x) (abs y))
                  (* (/ x (abs x)) (- (abs x) 1))
                  x))
   (check-eq? ((posn-y test-move-toward-1))
              (if (> (abs y) (abs x))
                  (* (/ y (abs y)) (- (abs y) 1))
                  y))
   (check-eq? ((posn-x test-move-toward-2))
              (if (> (abs x) (abs y))
                  0
                  x))
   (check-eq? ((posn-y test-move-toward-2))
              (if (> (abs y) (abs x))
                  0
                  y))
   ;; move
   (check-eq? ((posn-x ((posn-move result) 50 20)))
              (+ x 50))
   (check-eq? ((posn-y ((posn-move result) 50 20)))
              (+ y 20))
   ;; draw-on/image
   (check-true (image?
                ((posn-draw-on/image result)
                 (circle 1 "solid" "green")
                 (empty-scene 200 200))))
   ;; dist
   (check-posn-dist (new-posn 0 0) result)
   ))
;; tests for test-posn
(define testing-test-posn
  (test-posn (lambda () (new-posn 1 2)) 1 2 "testing test-posn"))
(run-tests testing-test-posn)


;; TESTS

;; tests for new-posn
(define new-posn-test-suite
  (test-suite
   "test for new-posn"
   ;; (0,0)
   ;; posn-x
   (test-posn (lambda () (new-posn 0 0)) 0 0 "origin")
   ;; (400, 500)
   (test-posn (lambda () (new-posn 400 500)) 400 500 "")
   ;; (500, 400)
   (test-posn (lambda () (new-posn 500 400)) 500 400 "")
   ;; (-500, 400)
   (test-posn (lambda () (new-posn -500 400)) -500 400 "")
   ;; (500, -400)
   (test-posn (lambda () (new-posn 500 -400)) 500 -400 "")
   ;; (-1, -1)
   (test-posn (lambda () (new-posn -100 -300)) -100 -300 "")
   ;; wrong inputs
   ;; -> error when doing move/toward
   (check-exn exn:fail? (lambda () ((posn-move (new-posn "" "") 50 50))))
   ;; -> error when doing move
   (check-exn exn:fail?
              (lambda ()
                ((posn-move-toward/speed (new-posn "" "")
                                         (new-posn 500 500) 50))))
   ;; error when doing dist
   (check-exn exn:fail?
              (lambda ()
                ((posn-dist (new-posn "" "")
                            (new-posn 500 500)))))
   ;; error when using the wrong accessor
   (check-exn exn:fail? (lambda () ((new-posn 0 0) "unknown accessor")))
   ))

;; run test suites
(run-tests new-posn-test-suite)


;; ------------------------------------------------------
;; NEW PLAYER

;; HELPERS

;; (test-player-draw-on player img)
;      - player is a player returned by (new-player ...)
;      - img is an image?
; returns test suite that checks behavior of player-draw-on
(define (test-player-draw-on player img msg)
  (define x ((posn-x ((player-posn player)))))
  (define y ((posn-y ((player-posn player)))))
  
  (test-suite
   msg
   (place-image-check (lambda () ((player-draw-on player) img))
                      PLAYER-IMG ;i1
                      x ;i2
                      y ;i3
                      img ;i4
                      "player-draw-on place-image-check")
   ))

;; test-player: (-> player) (-> posn) string -> test-suite
;; (test-player player-fxn posn-fxn msg)
;      - player-fxn is a lambda call to (new-player ...)
;      - posn-fxn is a lambda call to (new-posn ...)
;  the posn passed as posn-fxn must be the posn that the player is initialized with.
;; returns a test suite with name msg
(define (test-player player-fxn posn-fxn msg)
  ;; SETUP
  ; assume posn-fxn gives us a valid posn. we will not run (test-posn ...) on it.
  (define player (player-fxn))
  (define posn (posn-fxn))
  (define x ((posn-x posn)))
  (define y ((posn-y posn)))
  ;; TEST SUITE
  (test-suite
   msg
   (test-suite
    "player-posn"
    (check-true (procedure? (player-posn player)))
    (check-equal? (procedure-arity (player-posn player)) 0)
    (test-posn (lambda () ((player-posn player)))
               x y "player-posn is expected")
    )
   (test-suite
    "move-toward"
    (check-true (procedure? (player-move-toward player)))
    (check-equal? (procedure-arity (player-move-toward player)) 1)
    ; check posn of returned player is correct
    (check-true (posn-equal?
                 ; we use our algo-move-toward fxn to generated epected values
                 (algo-move-toward ((player-posn player)) (new-posn 1 2) PLAYER-SPEED)
                 ; we check if that's equal to actual values
                 ((player-posn ((player-move-toward player) (new-posn 1 2))))))
    (check-true (posn-equal? (algo-move-toward ((player-posn player)) (new-posn 0 0) PLAYER-SPEED)
                             ((player-posn ((player-move-toward player) (new-posn 0 0))))))
    (check-true (posn-equal? (algo-move-toward ((player-posn player)) (new-posn 100 150) PLAYER-SPEED)
                             ((player-posn ((player-move-toward player) (new-posn 100 150))))))
    (check-true (posn-equal? (algo-move-toward ((player-posn player)) (new-posn -5 20) PLAYER-SPEED)
                             ((player-posn ((player-move-toward player) (new-posn -5 20))))))
    (check-true (posn-equal? (algo-move-toward ((player-posn player)) (new-posn 25 16) PLAYER-SPEED)
                             ((player-posn ((player-move-toward player) (new-posn 25 16))))))
    (check-true (posn-equal? (algo-move-toward ((player-posn player)) (new-posn -5 -5) PLAYER-SPEED)
                             ((player-posn ((player-move-toward player) (new-posn -5 -5))))))
    )
   (test-suite
    "draw-on"
    (check-true (procedure? (player-draw-on player)))
    (check-equal? (procedure-arity (player-draw-on player)) 1)
    (test-player-draw-on player (empty-scene 200 300) "player on empty scene")
    ; (test-player-draw-on player (image '(PLAYER-IMG 5 6 (empty-scene 10 20))) "player on img w player")
    ; (test-player-draw-on player (image '(ZOMBIE-IMG 5 6 (empty-scene 10 20))) "player on img w zombie")
    ; (test-player-draw-on player (image '(ZOMBIE-IMG 10 20 (image '(PLAYER-IMG 5 6 (empty-scene 10 20))))) "player on img w zombie and player")
    (test-player-draw-on player (circle 200 "big" "blue") "player on circle")
    (test-player-draw-on player (place-image (circle 20 "small" "pink") 2 1 (empty-scene 5 500))
                         "player on place-image")
    )
   ; Note: cannot test "else" case ("unknown message") because code won't compile
   ))

;; TESTS

; test new-player
(define new-player-tests
  (test-suite
   "new-player-test"
   #:before (lambda () (display "START new-player-tests\n"))
   #:after  (lambda () (display "FINISH new-player-tests\n"))
   (test-suite
    "accepts 1 argument"
    (check-exn exn:fail? (lambda () (new-player)))
    (check-exn exn:fail? (lambda () (new-player 2 3 4)))
    (check-exn exn:fail? (lambda () (new-player 10 10)))
    (check-equal? (procedure-arity new-player) 1)
    )
   (test-suite
    "arg must be a valid posn"
    ;; TODO these tests are totally wrong lol
    (check-exn exn:fail? (lambda () (test-player (new-player "cat"))))
    (check-exn exn:fail? (lambda () (test-player (new-player 'fish))))
    (check-exn exn:fail? (lambda () (test-player (new-player (new-posn 3)))))
    (check-exn exn:fail? (lambda () (test-player (new-player (list 3 4 5)))))
    )
   (test-suite
    "valid instances"
    (test-player (lambda () (new-player (new-posn 5 0)))
                 (lambda () (new-posn 5 0))
                 "player at (5,0)")
    (test-player (lambda () (new-player (new-posn 0 0)))
                 (lambda () (new-posn 0 0))
                 "player at (0,0)")
    (test-player (lambda () (new-player (new-posn 50 90)))
                 (lambda () (new-posn 50 90))
                 "player at (50,90)")
    (test-player (lambda () (new-player (new-posn -1 -300)))
                 (lambda () (new-posn -1 -300))
                 "player at (-1,-300)")
    (test-player (lambda () (new-player (new-posn 80 -10)))
                 (lambda () (new-posn 80 -10))
                 "player at (80,-10)")
    )
   ))

(run-tests new-player-tests)

;; ---------------------------------------------------------
;; NEW ZOMBIE

;; UTILITIES
;; zombie-equal?: Zombie Zombie -> Boolean
(define (zombie-equal? zombie-1 zombie-2)
  (posn-equal? ((zombie-posn zombie-1))
               ((zombie-posn zombie-2))))
;; tests for zombie-equal? 
; (check-true (zombie-equal? (new-zombie (new-posn 0 0))
;                            (new-zombie (new-posn 0 0))))
; (check-false (zombie-equal? (new-zombie (new-posn 0 1))
;                            (new-zombie (new-posn 0 0))))

; algo-move-toward-for-zombie - takes a posn and a zombie, and returns a new zombie
; Zombie Posn -> Zombie
; from-zombie: the original zombie
; to-posn: the posn the original zombie is moving towards
(define (algo-move-toward-for-zombie from-zombie to-posn)
  ;; get from-posn
  (define from-posn ((zombie-posn from-zombie)))
  ;; define speed
  (define speed ZOMBIE-SPEED)
  ;; create the new zombie
  (new-zombie (algo-move-toward from-posn to-posn speed)))
;; test zombie-algo-move-toward
; (check-true (zombie-equal? (algo-move-toward-for-zombie (new-zombie (new-posn 0 0))
;                                                   (new-posn 50 60))
;                            (new-zombie (new-posn 0 ZOMBIE-SPEED))))
; (check-false (zombie-equal? (algo-move-toward-for-zombie (new-zombie (new-posn 0 0))
;                                                         (new-posn 40 30))
;                            (new-zombie (new-posn 0 ZOMBIE-SPEED))))

;; is-zombie-really-touching?: Zombie Posn -> Boolean
(define (is-zombie-really-touching? z p)
  ;; zombie's posn
  (define z-posn ((zombie-posn z)))
  ;; dist
  (<= ((posn-dist p) z-posn) ZOMBIE-RADIUS))
;; tests for is-zombie-really-touching?
; (check-true (is-zombie-really-touching? (new-zombie (new-posn 0 0)) 
;                                         (new-posn ZOMBIE-RADIUS 0)))
; (check-false (is-zombie-really-touching? (new-zombie (new-posn 0 0))
;                                          (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS)))

;; place-zombie-image-check Zombie string? scene -> void
;; checks whether a given zombie z draws properly with
;; a given color c and scene s 
(define (place-zombie-image-check z c s)
  (define z-posn ((zombie-posn z)))
  (place-image-check (lambda () ((zombie-draw-on/color z) c s))
                      (circle ZOMBIE-RADIUS "solid" c)
                      ((posn-x z-posn))
                      ((posn-y z-posn))
                      s
                      ""))

;; test-zombie: (-> Zombie) Real Real String -> Void
;; generates and runs a test suite with message msg, 
;; testing the result of z-fxn against the Zombie 
;; created with position (x, y)
(define (test-zombie z-fxn x y msg)
  (define result (z-fxn))
  (define posn-touching-z ((posn-move ((zombie-posn result))) ZOMBIE-RADIUS 0))
  (define posn-not-touching-z ((posn-move ((zombie-posn result))) ZOMBIE-RADIUS ZOMBIE-RADIUS))
  (test-suite
   msg
   ;; tests
   ;; posn
   (posn-equal? ((zombie-posn result))
                (new-posn x y))
   ;; draw-on/color
  (place-zombie-image-check result "red" MT-SCENE)

   ;; touching?
   (check-eq? (is-zombie-really-touching? result posn-touching-z)
              ((zombie-touching? result) posn-touching-z))
   (check-eq? (is-zombie-really-touching? result posn-not-touching-z)
              ((zombie-touching? result) posn-not-touching-z))           
   ;; move-toward
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn 0 0))
                              ((zombie-move-toward result) (new-posn 0 0))))
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn -300000 -40000))
                              ((zombie-move-toward result) (new-posn -300000 -40000))))
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn 30000 30000))
                              ((zombie-move-toward result) (new-posn 30000 30000))))
   ))


;; place-images-recursively: List (Any -> Image) string? Image -> Image
;; helper for place-zombies-image-check
(define (place-images-recursively l i-fxn x-fxn y-fxn s)
  (if (empty? l)
      s
      (local [(define obj (first l))]
        (place-image (i-fxn obj)
                    (x-fxn obj)
                    (y-fxn obj)
                    (place-images-recursively (rest l)
                                              i-fxn
                                              x-fxn
                                              y-fxn
                                              s)))))

;; place-zombies-image-check: Zombies (listof zombie) string image -> void
;; checks whether a list of zombies zs (should incl. mt-zombie) 
;; draws properly with color c and scene s
(define (place-zombies-image-check actual-zs zs c s)
  (define img (place-images-recursively zs (lambda (z)
                                              (circle ZOMBIE-RADIUS "solid" c))
                                           (lambda (z)
                                              ((posn-x ((zombie-posn z)))))
                                           (lambda (z)
                                              ((posn-y ((zombie-posn z)))))
                                           s))
  (test-image-equal ((zombies-draw-on/color actual-zs) c s)
                    img))

;; zombies-really-touching?: (listof zombie) posn -> Boolean 
;; discerns whether a list of zombie zs is touching a
;; posn p
(define (zombies-really-touching? zs p)
  (cond [(empty? zs)
         #f]
        [(is-zombie-really-touching? (first zs) p)
         #t]
        [else
         (zombies-really-touching? (rest zs) p)]))


;; convert-list-to-cons-zombie: (listof zombie) -> Zombies
(define (convert-list-to-cons-zombie list-zs)
  (if (empty? list-zs)
      (new-mt-zombies)
      (begin
        (new-cons-zombies (first list-zs)
                (convert-list-to-cons-zombie (rest list-zs))))))


;; equal-images: Image Image -> Boolean
(define (equal-images? img1 img2)
  (unless (image? img1)
    (error "img1 not an image"))
  (unless (image? img1)
    (error "img2 not an image"))
  (cond
    ; both lists
    [(and (list? (image-impl img1))
          (list? (image-impl img2)))
     (cond
       ; equal length
       [(= (length (image-impl img1))
           (length (image-impl img2)))
        (define l (length (image-impl img1)))
        (define bool #t)
        (for ([a (in-list (image-impl img1))]
              [b (in-list (image-impl img2))])
          (cond [(and (image? a) (image? b))
                (unless (equal-images? a b)
                  (set! bool #f))]
                [(and (not (image? a))
                      (not (image? b)))
                (unless (equal? a b)
                  (set! bool #f))]
                [else (set! bool #f)]))
        bool]
       ; unequal length
       [else (fail "lists are not equal length")]
       )]
    ; both cons and not lists
    [(and (cons? (image-impl img1))
          (cons? (image-impl img2))
          (not (list? (image-impl img1)))
          (not (list? (image-impl img2))))   
     (and 
      (equal? (car (image-impl img1))
                    (car (image-impl img2)))
      (equal? (cdr (image-impl img1))
                    (cdr (image-impl img2))))]
    ; not both cons or both lists
    [else (fail "image-impl not both cons or not both lists")]
  ))

; (check-false (equal-images? (place-image (circle ZOMBIE-RADIUS "solid" "red")
;                                         100
;                                         100
;                                         MT-SCENE)
;                            (place-image (circle ZOMBIE-RADIUS "solid" "yellow")
;                                         100
;                                         100
;                                         MT-SCENE)))
; (check-true (image? (circle ZOMBIE-RADIUS "solid" "red")))
; (check-true (list? (image-impl (circle ZOMBIE-RADIUS "solid" "red"))))

;; equal-zombies?: Zombies Zombies -> Boolean
;; if the images produced by zs1 and zs2 on 
;; an empty scene are the same, then
;; they are equal
(define (equal-zombies? zs1 zs2)
  (equal-images? ((zombies-draw-on/color zs1) "red" MT-SCENE)
                 ((zombies-draw-on/color zs2) "red" MT-SCENE)))
;; test for equality
; (check-true (equal-zombies? (new-mt-zombies) (new-mt-zombies)))
; (check-true (equal-zombies? (new-cons-zombies (new-zombie (new-posn 0 0))
;                                               (new-mt-zombies))
;                             (new-cons-zombies (new-zombie (new-posn 0 0))
;                                               (new-mt-zombies))))

;; kill-all-zombies: (listof zombie) (listof zombie) -> (listof (listof zombie))
;; kills all the zombies in undead-zombies if they
;; come in contact with dead-zombies without using 
;; higher-order recursive functions to test zombie 
;; functions properly
(define (kill-all-zombies undead-zombies dead-zombies)
  (cond [(empty? undead-zombies)
         (list undead-zombies dead-zombies)]
        [(or (zombies-really-touching? (rest undead-zombies) 
                                       ((zombie-posn (first undead-zombies))))
             (zombies-really-touching? dead-zombies
                                       ((zombie-posn (first undead-zombies)))))
         (kill-all-zombies (rest undead-zombies) (cons (first undead-zombies) dead-zombies))]
        [else
         (define res (kill-all-zombies (rest undead-zombies) dead-zombies))
         (list (cons (first undead-zombies)
                     (first res))
               (second res))]))

;; equal-hordes? Horde Horde -> Boolean
;; approximates equality by checking if
;; the images produced by the Hordes
;; on an empty scene are equal
(define (equal-hordes? h1 h2)
  (equal-images? ((horde-draw-on h1) MT-SCENE)
          ((horde-draw-on h2) MT-SCENE)))
;; test for equality
; (check-true (equal-hordes? (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
;                                                         (new-mt-zombies))
;                                       (new-mt-zombies))
;                            (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
;                                                         (new-mt-zombies))
;                                       (new-mt-zombies))))

;; test-new-mt-zombies
(define (test-new-mt-zombies z-fxn msg)
  (define result (z-fxn))
  (define real-zombie-list 
          (new-cons-zombies (new-zombie (new-posn 0 0))
                            (new-cons-zombies (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                                              (new-cons-zombies (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))
                                                                (new-mt-zombies)))))
  (define fake-zombie-list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                             (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))))
  (define h-list (kill-all-zombies '() fake-zombie-list))
  (define h (new-horde (convert-list-to-cons-zombie (first h-list))
                      (convert-list-to-cons-zombie (second h-list))))
  (define h2-list (kill-all-zombies fake-zombie-list '()))
  (define h2 (new-horde (convert-list-to-cons-zombie (first h2-list))
                        (convert-list-to-cons-zombie (second h2-list))))
  (test-suite
   msg
   ;; move-toward check
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn 0 0)) (new-mt-zombies)))
   ;; draw-on/color check
   (place-zombies-image-check result '() "red" MT-SCENE)
   ;; touching?
   (check-equal? (zombies-really-touching? '() (new-posn 0 0))
                   ((zombies-touching? result) (new-posn 0 0)))
   ;; kill-all
   (check-true (equal-hordes? h ((zombies-kill-all result) real-zombie-list)))
   (check-false (equal-hordes? h ((zombies-kill-all real-zombie-list) result)))
   (check-true (equal-hordes? h2 ((zombies-kill-all real-zombie-list) result)))
   ))

;; algo-move-toward-for-zombies: (listof zombie) -> (listof zombie)
(define (algo-move-toward-for-zombies zs p)
  (if (empty? zs)
      empty
      (cons (algo-move-toward-for-zombie (first zs) p)
            (algo-move-toward-for-zombies (rest zs) p))))

;; test-new-cons-zombies: z-fxn (listof zombie) -> test-suite
(define (test-new-cons-zombies z-fxn zs msg)
  ;; define result
  (define result (z-fxn))
  ;; define important local variables
  ;; test suite
  (define real-zombie-list 
          (new-cons-zombies (new-zombie (new-posn 0 0))
                            (new-cons-zombies (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                                              (new-cons-zombies (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))
                                                                (new-mt-zombies)))))
  (define fake-zombie-list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                             (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))))
  (define h-list (kill-all-zombies zs fake-zombie-list))
  (define h (new-horde (convert-list-to-cons-zombie (first h-list))
                      (convert-list-to-cons-zombie (second h-list))))
  (define h2-list (kill-all-zombies fake-zombie-list zs))
  (define h2 (new-horde (convert-list-to-cons-zombie (first h2-list))
                        (convert-list-to-cons-zombie (second h2-list))))
  (test-suite
   msg
   ;; test move-toward
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn 0 0))
                               (convert-list-to-cons-zombie 
                                (algo-move-toward-for-zombies zs (new-posn 0 0)))))
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn -500 -500))
                               (convert-list-to-cons-zombie 
                                (algo-move-toward-for-zombies zs (new-posn -500 -500)))))
   (check-true (equal-zombies? ((zombies-move-toward result) (new-posn 100 1400))
                               (convert-list-to-cons-zombie 
                                (algo-move-toward-for-zombies zs (new-posn 100 1400)))))
   ;; test draw-on-color
   (place-zombies-image-check result zs "red" MT-SCENE)
   ;; test touching?
   (check-equal? (zombies-really-touching? zs (new-posn 0 0))
                 ((zombies-touching? result) (new-posn 0 0)))
   (check-equal? (zombies-really-touching? zs (new-posn -500 -500))
                 ((zombies-touching? result) (new-posn -500 -500)))
   (check-equal? (zombies-really-touching? zs (new-posn 300 300))
                 ((zombies-touching? result) (new-posn 300 300)))
   ;; test kill-all
   (check-true (equal-hordes? h ((zombies-kill-all result) real-zombie-list)))
   (check-false (equal-hordes? h ((zombies-kill-all real-zombie-list) result)))
   (check-true (equal-hordes? h2 ((zombies-kill-all real-zombie-list) result)))
   ))

;; test-new-cons-zombies
(run-tests (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-cons-zombies (new-zombie (new-posn 300 300))
                                                              (new-mt-zombies))))
                       (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 300 300)))
                       ""))


;; algo-horde-move-toward: (listof (listof zombie)) Posn -> (listof (listof zombie))
;; h: '(undead dead)
;; pos: posn
;; undead zombies in the horde h are moving towards posn pos
(define (algo-horde-move-toward h pos)
  (list (algo-move-toward-for-zombies (first h) pos) 
        (second h)))

;; test-new-horde: lambda (listof (listof zombie)) string -> test-suite
;; h-fxn: function that creates a new horde
;; h-list: '(dead undead)
;; msg: test-suite message
(define (test-new-horde h-fxn h-list msg)
  (define result (h-fxn))
  (define expected-dead-scene (place-images-recursively (second h-list)
                                                        (lambda (z)
                                                          (circle ZOMBIE-RADIUS "solid" "black"))
                                                        (lambda (z)
                                                          ((posn-x ((zombie-posn z)))))
                                                        (lambda (z)
                                                          ((posn-y ((zombie-posn z)))))
                                                        MT-SCENE))

  (define move-toward-1 (algo-horde-move-toward h-list (new-posn 0 0)))
  (define move-toward-2 (algo-horde-move-toward h-list (new-posn 450 -100000)))
  (define kill-all-1 (kill-all-zombies (first h-list) (second h-list)))
  (test-suite
   msg
   ;; tests
   ;; dead
   (check-true (equal-zombies? ((horde-dead result))
                               (convert-list-to-cons-zombie (second h-list))))
   ;; undead
   (check-true (equal-zombies? ((horde-undead result))
                               (convert-list-to-cons-zombie (first h-list))))
   ;; draw-on
   (check-true (equal-images? 
          ;; result image
          ((horde-draw-on result) MT-SCENE)
          ;; h-list image
          (place-images-recursively (first h-list)
                                    (lambda (z)
                                      (circle ZOMBIE-RADIUS "solid" "yellow"))
                                    (lambda (z)
                                      ((posn-x ((zombie-posn z)))))
                                    (lambda (z)
                                      ((posn-y ((zombie-posn z)))))
                                    expected-dead-scene)
          ))
   ;; move-toward
   (check-true (equal-hordes? ((horde-move-toward result) (new-posn 0 0))
                              (new-horde (convert-list-to-cons-zombie (first move-toward-1))
                                          (convert-list-to-cons-zombie (second move-toward-1)))))
                  
   (check-true (equal-hordes? ((horde-move-toward result) (new-posn 450 -100000))
                 (new-horde (convert-list-to-cons-zombie (first move-toward-2))
                            (convert-list-to-cons-zombie (second move-toward-2)))))
   ;; kill-all
   (check-true (equal-hordes? ((horde-eat-brains result))
                              (new-horde (convert-list-to-cons-zombie (first kill-all-1))
                                         (convert-list-to-cons-zombie (second kill-all-1)))))
                  ))


;; TESTS
;; tests for new-zombie
(define new-zombie-test-suite
  (test-suite
    "test for new-zombie"
    ;; tests using test-zombie
    (test-zombie (lambda () (new-zombie (new-posn 0 0))) 0 0 "")
    (test-zombie (lambda () (new-zombie (new-posn 500 300))) 500 300 "")
    (test-zombie (lambda () (new-zombie (new-posn -100 100))) -100 100 "")
    (test-zombie (lambda () (new-zombie (new-posn -30000 -500))) -30000 -500 "")
    (test-zombie (lambda () (new-zombie (new-posn 500 -500))) 500 -500 "")
    ;; test for wrong inputs
    (check-exn exn:fail? (lambda () ((new-zombie ""))))
    ;; (check-exn exn:fail? (lambda () ((zombie-posn (new-posn 0 0)))))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color "red") "red" 5)))
    (check-exn exn:fail? (lambda () ((zombie-touching? (new-posn 5 5)))))
    (check-exn exn:fail? (lambda () ((zombie-move-toward (new-zombie (new-posn 0 0))))))
    ))

(define new-cons-zombies-test-suite
  (test-suite "test for new-cons-zombies"
   ;; tests using test-new-cons-zombies
   (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-cons-zombies (new-zombie (new-posn 300 300))
                                                              (new-mt-zombies))))
                       (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 300 300)))
                       "")
                       
    (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn -1000 -100))
                                            (new-cons-zombies (new-zombie (new-posn 300 300))
                                                              (new-mt-zombies))))
                       (list (new-zombie (new-posn -1000 -100)) (new-zombie (new-posn 300 300)))
                       "")
    (test-new-cons-zombies (lambda ()
                          (new-cons-zombies (new-zombie (new-posn -1000 -100))
                                            (new-mt-zombies)))
                       (list (new-zombie (new-posn -1000 -100)))
                       "")
    (test-new-cons-zombies (lambda ()
                              (new-cons-zombies (new-zombie (new-posn -1000 -100))
                                            (new-mt-zombies)))
                             (list (new-zombie (new-posn -1000 -100)))
                       "")
    (test-new-cons-zombies (lambda ()
                              (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-mt-zombies)))
                             (list (new-zombie (new-posn 0 0)))
                       "")
    (test-new-cons-zombies (lambda ()
                              (new-cons-zombies (new-zombie (new-posn 0 0))
                                            (new-cons-zombies (new-zombie (new-posn 3000 -300))
                                                              (new-cons-zombies (new-zombie (new-posn 3000 -300))
                                                                                (new-mt-zombies)))))
                             (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 3000 -300)) (new-zombie (new-posn 3000 -300)))
                       "")
    ;; test for wrong inputs
    (check-exn exn:fail? (lambda () ((new-cons-zombies ""))))
    ;; (check-exn exn:fail? (lambda () ((zombie-posn (new-posn 0 0)))))
    (check-exn exn:fail? (lambda () ((zombies-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombies-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombies-draw-on/color "red") "red" 5)))
    (check-exn exn:fail? (lambda () ((zombies-touching? (new-posn 5 5)))))
    (check-exn exn:fail? (lambda () ((zombies-move-toward (new-zombie (new-posn 0 0))))))
    ))

(define new-horde-test-suite
  (test-suite
   "tests for new-horde"
   ;; tests
   ;; horde with empty undead, filled dead
   (test-new-horde (lambda ()
                 (new-horde (new-mt-zombies)
                            (new-cons-zombies (new-zombie (new-posn 0 0))
                                              (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                (new-mt-zombies)))))
               (list (list)
                     (list (new-zombie (new-posn 0 0))
                           (new-zombie (new-posn 100 5000))))
               "")
   
   ;; horde with filled undead, empty dead
   (test-new-horde (lambda ()
                    (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
                                                  (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                    (new-mt-zombies)))
                                (new-mt-zombies)))
                  (list (list (new-zombie (new-posn 0 0))
                              (new-zombie (new-posn 100 5000)))
                        (list))
               "")
   ;; horde with empty, empty
   (test-new-horde (lambda ()
                    (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
                                                  (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                    (new-mt-zombies)))
                                (new-mt-zombies)))
                  (list (list (new-zombie (new-posn 0 0))
                              (new-zombie (new-posn 100 5000)))
                        (list))
               "")
   ;; horde with dead overlapping
   (test-new-horde (lambda ()
                      (new-horde (new-cons-zombies (new-zombie (new-posn 0 0))
                                                    (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                      (new-mt-zombies)))
                                 (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                   (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                                     (new-mt-zombies)))))
                   (list (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 100 5000)))
                         (list (new-zombie (new-posn -1000 -1000)) (new-zombie (new-posn -1000 -1000))))
                   "")
  ;  ;; horde with undead overlapping
  (test-new-horde (lambda ()
                      (new-horde (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                   (new-cons-zombies (new-zombie (new-posn -1000 -1000))
                                                                     (new-mt-zombies)))
                                 (new-cons-zombies (new-zombie (new-posn 0 0))
                                                    (new-cons-zombies (new-zombie (new-posn 100 5000))
                                                                      (new-mt-zombies)))))
                   (list (list (new-zombie (new-posn -1000 -1000)) (new-zombie (new-posn -1000 -1000)))
                         (list (new-zombie (new-posn 0 0)) (new-zombie (new-posn 100 5000))))
                   "")
   ))

(run-tests new-zombie-test-suite)
(run-tests (test-new-mt-zombies (lambda () (new-mt-zombies)) ""))
(run-tests new-cons-zombies-test-suite)
(run-tests new-horde-test-suite)

;; -----------------------------------------------------------
;; NEW WORLD

;; HELPERS

;; player-ticks: Player Posn Number -> Player
;    - player is the original player that we are moving
;    - posn is where we are moving towards (mouse)
;    - num-ticks is the number of ticks to simulate (number of times to move the player)
; returns a new Player that's been moved based on mouse and num-ticks.
(define (player-ticks player posn num-ticks)
  (cond
    ;base case: no ticks left, return player
    [(= num-ticks 0) player]
    ;recursive case: call with moved player and decremented ticks
    [else
     (player-ticks (new-player (algo-move-toward ((player-posn player)) posn PLAYER-SPEED))
                   posn
                   (- num-ticks 1))]
    ))

;; TODO: relace with horde movement algo from akash
;; horde-ticks: Horde Player Number -> Horde
;    - horde is the original horde that we are moving
;    - player is the Player the horde is moving towards
;    - num-ticks is the number of ticks to simulate (number of times to move the horde)
; returns a new Horde that's been moved based on player and num-ticks.
(define (horde-ticks horde player num-ticks)
  (cond
    ;base case: no ticks left, return horde
    [(= num-ticks 0) horde]
    ;recursive case: call with moved horde and decremented ticks
    [else
     (horde-ticks ((horde-move-toward ((horde-eat-brains horde))) ((player-posn player)))
                   player
                   (- num-ticks 1))]
    ))

;; world-ticks: World Number -> World
;    - world is the original World that we are changing
;    - num-ticks is the number of ticks to simulate (number of times to update world)
; returns a new World that's been changed based on num-ticks
(define (world-ticks world num-ticks)
  (cond
    ;base case: no ticks left, return world
    [(= num-ticks 0) world]
    ;recursive case: call with updated world and decremented ticks
    [else
     (world-ticks ((world-on-tick world))
                  (- num-ticks 1))]
    ))

;; test-world-on-tick: World Player Horde Posn String -> test-suite
;    - world is a World whose on-tick function we're testing
;    - player and horde are the Player and Horde used to initialize world
;    - mouse is the location of the mouse
; assumes world's on-tick fxn has correct arity and param types
(define (test-world-on-tick world player horde mouse msg)
  (test-suite
   msg
   ; world looks right
   (test-image-equal ((world-to-draw world))
                     ((player-draw-on player) ((horde-draw-on horde) MT-SCENE)))
   ; world after 1 tick looks right
   (test-image-equal ((world-to-draw (world-ticks world 1)))
                     ((player-draw-on (player-ticks player mouse 1))
                      ((horde-draw-on (horde-ticks horde player 1)) MT-SCENE)))
   ; world after 2 tick looks right
   (test-image-equal ((world-to-draw (world-ticks world 2)))
                     ((player-draw-on (player-ticks player mouse 2))
                      ((horde-draw-on (horde-ticks horde player 2)) MT-SCENE)))
   ; world after 3 tick looks right
   (test-image-equal ((world-to-draw (world-ticks world 3)))
                     ((player-draw-on (player-ticks player mouse 3))
                      ((horde-draw-on (horde-ticks horde player 3)) MT-SCENE)))
   ; world after 10 tick looks right
   (test-image-equal ((world-to-draw (world-ticks world 10)))
                     ((player-draw-on (player-ticks player mouse 10))
                      ((horde-draw-on (horde-ticks horde player 10)) MT-SCENE)))
   ))

;; test-world-on-mouse: World Player Horde Number Number String String -> test-suite
;    - let world be any World returned by (new-world ...)
;    - wom is the World returned by ((world-on-mouse world) ...)
;    - player and horde are the Player and Horde used to initialize world
;    - x, y, and str are args passed to the call to ((world-on-mouse world) ...)
; assumes world's on-mouse fxn has correct arity and param types
(define (test-world-on-mouse wom player horde x y str msg)
  (define mouse (if (equal? str "leave")
                    ((player-posn player))
                    (new-posn x y)))
  (test-suite
   msg
   (check-true (procedure? wom))
   (check-equal? (procedure-arity wom) 1)
   (check-true (procedure? (world-on-tick wom)))
   (check-equal? (procedure-arity (world-on-tick wom)) 0)
   (check-true (procedure? (world-to-draw wom)))
   (check-equal? (procedure-arity (world-to-draw wom)) 0)
   (test-world-on-tick wom player horde mouse "test-world-on-mouse testing on-tick")
   ))

;; test-world/args : World Player Posn Horde String -> test-suite
; inputs are assumed to be valid:
;      - world is a World returned by a call to (new-world ...)
;      - player is a Player returned by a call to (new-player ...)
;      - mouse is a Posn returned by a call to (new-posn ...)
;      - horde is a Horde returned by a call to (new-horde ...)
; returns test-suite that checks if a World is a valid world
; based on the args passed as inputs when initializing this World.
(define (test-world/args world player mouse horde msg)
  (test-suite
   msg
   (test-suite
    "a world is a procedure"
    (check-true (procedure? world))
    (check-equal? (procedure-arity world) 1)
    )
   (test-suite
    "world-to-draw"
    (check-true (procedure? (world-to-draw world)))
    (check-equal? (procedure-arity (world-to-draw world)) 0)
    (check-true (image? ((world-to-draw world))))
    (test-image-equal ((world-to-draw world));actual
                      ((player-draw-on player) ((horde-draw-on horde) MT-SCENE)));expected
    )
   (test-suite
    "world-on-tick"
    (check-true (procedure? (world-on-tick world)))
    (check-equal? (procedure-arity (world-on-tick world)) 0)
    (test-world-on-tick world player horde mouse "test-world/args testing on-tick")
    )
   (test-suite
    "world-on-mouse" ; Real Real String -> World
    (check-true (procedure? (world-on-mouse world)))
    (check-equal? (procedure-arity (world-on-mouse world)) 3)
    (test-world-on-mouse ((world-on-mouse world) 6 10 "")
                         player horde 6 10 "" "wom-1")
    (test-world-on-mouse ((world-on-mouse world) 12 -1 "")
                         player horde 12 -1 "" "wom-2")
    (test-world-on-mouse ((world-on-mouse world) -1 5 "dog")
                         player horde -1 5 "dog" "wom-3")
    (test-world-on-mouse ((world-on-mouse world) 0 0 "leave")
                         player horde 0 0 "leave" "wom-4")
    (test-world-on-mouse ((world-on-mouse world) 200 150 "leave")
                         player horde 200 150 "leave" "wom-5")
    )
   (test-suite
    "world-stop-when"
    ; this is never used??? not gonna test it lol
    (check < 6 7)
    )
   )
  )

;; test-world-on-mouse/noargs: World World Number Number String String -> test-suite
;    - world is a World returned by (new-world ...)
;    - wom is the World returned by ((world-on-mouse world) ...)
;    - x, y, and str are the args passed to the call to ((world-on-mouse world) ...)
; assumes world's on-mouse fxn has correct arity and param types
(define (test-world-on-mouse/noargs world wom x y str msg)
  (define player (new-player (new-posn (second (image-impl ((world-to-draw world))))
                                       (third (image-impl ((world-to-draw world)))))))
  (define mouse (if (equal? str "leave")
                    ((player-posn player))
                    (new-posn x y)))
  (test-suite
   msg
   ; wom is a World type
   (check-true (procedure? wom))
   (check-equal? (procedure-arity wom) 1)
   (check-true (procedure? (world-on-tick wom)))
   (check-equal? (procedure-arity (world-on-tick wom)) 0)
   (check-true (procedure? (world-to-draw wom)))
   (check-equal? (procedure-arity (world-to-draw wom)) 0)
   ; wom looks the exact same as world
   (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw wom)))))
   (check-equal? (second (image-impl ((world-to-draw world))))
                 (second (image-impl ((world-to-draw wom)))))
   (check-equal? (third (image-impl ((world-to-draw world))))
                 (third (image-impl ((world-to-draw wom)))))
   (test-image-equal (fourth (image-impl ((world-to-draw world))))
                     (fourth (image-impl ((world-to-draw wom)))))
   ; test ticks
   (test-world-on-tick/mouse wom mouse msg)
   ))

;; test-world-on-tick/mouse: World Posn String -> test-suite
;    - world is a World returned by (new-world ...)
;    - mouse is the current mouse location
; assumes world's on-tick fxn has correct arity and param types
(define (test-world-on-tick/mouse wom mouse msg)
  (define player (new-player (new-posn (second (image-impl ((world-to-draw wom))))
                                       (third (image-impl ((world-to-draw wom)))))))
  (define (test-n-ticks n)
    (define player-n (player-ticks player mouse n))
    (define world-n (world-ticks wom n))
    (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw world-n)))))
    (check-equal? ((posn-x ((player-posn player-n))))
                  (second (image-impl ((world-to-draw world-n)))))
    (check-equal? ((posn-y ((player-posn player-n))))
                  (third (image-impl ((world-to-draw world-n)))))
    (check-true (image? (fourth (image-impl ((world-to-draw world-n))))))
  )
  (test-suite
   msg
   (test-n-ticks 1)
   (test-n-ticks 2)
   (test-n-ticks 3)
   (test-n-ticks 6)
   (test-n-ticks 10)
   ))

;; test-world/no-args : World -> test-suite
;      - world is a World returned by (new-world ...)
; a lighter, less exhaustive test. while test-world/args allows you to check that
; the world-... fxns return both the correct Type AND value, test-world/noargs only
; checks that the world-... fxns return the correct Type.
; this should only be used to check the validity of a World when you don't know
; what values were used to initalize the World.
(define (test-world/no-args world msg)
  (test-suite
   msg
   (test-suite
    "a world is a procedure"
    (check-true (procedure? world))
    (check-equal? (procedure-arity world) 1)
    )
   (test-suite
    "world-to-draw"
    (check-true (procedure? (world-to-draw world)))
    (check-equal? (procedure-arity (world-to-draw world)) 0)
    (check-true (image? ((world-to-draw world))))
    (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw world)))))
    (check-true (real? (second (image-impl ((world-to-draw world))))))
    (check-true (real? (third (image-impl ((world-to-draw world))))))
    (check-true (image? (fourth (image-impl ((world-to-draw world))))))
    )
   (test-suite
    "world-on-tick"
    (check-true (procedure? (world-on-tick world)))
    (check-equal? (procedure-arity (world-on-tick world)) 0)
    ; world-on-tick produces a world, which is a procedure
    (check-true (procedure? ((world-on-tick world))))
    (check-equal? (procedure-arity ((world-on-tick world))) 1)
    ; new world looks right
    (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw ((world-on-tick world)))))))
    (check-true (real? (second (image-impl ((world-to-draw ((world-on-tick world))))))))
    (check-true (real? (third (image-impl ((world-to-draw ((world-on-tick world))))))))
    (check-true (image? (fourth (image-impl ((world-to-draw ((world-on-tick world))))))))
    )
   (test-suite
    "world-on-mouse"
    (check-true (procedure? (world-on-mouse world)))
    (check-equal? (procedure-arity (world-on-mouse world)) 3)
    ; world-on-mouse produces a world, which is a procedure
    (check-true (procedure? ((world-on-mouse world) 1 2 "")))
    (check-equal? (procedure-arity ((world-on-mouse world) 1 2 "")) 1)
    ; new world looks right - same player and horde but new mouse, which changes tick behavior.
    (test-world-on-mouse/noargs world ((world-on-mouse world) 1 2 "")
                                1 2 ""
                                "1 2 blank")
    (test-world-on-mouse/noargs world ((world-on-mouse world) -1 -8 "")
                                -1 -8 ""
                                "-1 -8 blank")
    (test-world-on-mouse/noargs world ((world-on-mouse world) 0 -120 "")
                                0 -120 ""
                                "0 -120 blank")
    (test-world-on-mouse/noargs world ((world-on-mouse world) 57 6 "leave")
                                57 6 "leave"
                                "57 6 leave")
    (test-world-on-mouse/noargs world ((world-on-mouse world) -2 49 "leave")
                                -2 49 "leave"
                                "-2 49 leave")
    )
   )
  )
  

;; TESTS

(define new-world-tests
  (test-suite
   "new-world-tests"
   #:before (lambda () (display "START new-world-tests\n"))
   #:after  (lambda () (display "FINISH new-world-tests\n"))
   (test-suite
    "accepts 3 args only"
    (check-true (procedure? new-world))
    (check-equal? (procedure-arity new-world) 3)
    )
   (test-suite
    "player, posn, horde"
    ;; TODO: how do you test restrictions on inputs? try running stuff and doesn't work?
    ;; maybe helper for this
    )
   (test-suite
    "produces valid world"
    (test-world/args (new-world (new-player (new-posn 5 6))
                                (new-posn 0 9)
                                (new-horde (new-cons-zombies (new-zombie (new-posn 10 20))
                                                             (new-cons-zombies (new-zombie (new-posn 15 16))
                                                                               (new-mt-zombies)))
                                           (new-cons-zombies (new-zombie (new-posn 30 31))
                                                             (new-mt-zombies))))
                     (new-player (new-posn 5 6))
                     (new-posn 0 9)
                     (new-horde (new-cons-zombies (new-zombie (new-posn 10 20))
                                                  (new-cons-zombies (new-zombie (new-posn 15 16))
                                                                    (new-mt-zombies)))
                                (new-cons-zombies (new-zombie (new-posn 30 31))
                                                  (new-mt-zombies)))
                     "world 1 test")
    (test-world/args (new-world (new-player (new-posn 17 600))
                                (new-posn 134 103)
                                (new-horde (new-cons-zombies (new-zombie (new-posn 18 590))
                                                             (new-cons-zombies (new-zombie (new-posn 15 160))
                                                                               (new-mt-zombies)))
                                           (new-cons-zombies (new-zombie (new-posn 0 0))
                                                             (new-cons-zombies (new-zombie (new-posn 400 16))
                                                                               (new-mt-zombies)))))
                     (new-player (new-posn 17 600))
                     (new-posn 134 103)
                     (new-horde (new-cons-zombies (new-zombie (new-posn 18 590))
                                                             (new-cons-zombies (new-zombie (new-posn 15 160))
                                                                               (new-mt-zombies)))
                                           (new-cons-zombies (new-zombie (new-posn 0 0))
                                                             (new-cons-zombies (new-zombie (new-posn 400 16))
                                                                               (new-mt-zombies))))
                     "world 2 test")
    (test-world/args (new-world (new-player (new-posn 1 3))
                                (new-posn 0 6)
                                (new-horde (new-cons-zombies (new-zombie (new-posn 0 5))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies)))
                     (new-player (new-posn 1 3))
                     (new-posn 0 6)
                     (new-horde (new-cons-zombies (new-zombie (new-posn 0 5))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies))
                     "world 3 test")
    (test-world/args (new-world (new-player (new-posn -50 23))
                                (new-posn -50 23)
                                (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies)))
                     (new-player (new-posn -50 23))
                     (new-posn -50 23)
                     (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                  (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                    (new-mt-zombies)))
                                (new-mt-zombies))
                     "world 4 test")
    )
   )
  )

; checks if w0 is a valid world
(define w0-tests
  (test-suite
   "w0-tests"
   #:before (lambda () (display "START w0-tests\n"))
   #:after  (lambda () (display "FINISH w0-tests\n"))
  (test-world/no-args w0 "w0 test")
  )
  )

; more tests of test-world/no-args
(define world/no-args-tests
  (test-suite
   "test-world/no-args tests"
   #:before (lambda () (display "START test-world/no-args tests\n"))
   #:after  (lambda () (display "FINISH test-world/no-args tests\n"))
   (test-world/no-args (new-world (new-player (new-posn -50 23))
                                (new-posn -50 23)
                                (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies)))
                       "world 1 no arg test")
   (test-world/no-args (new-world (new-player (new-posn -50 23))
                                  (new-posn -50 23)
                                  (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                               (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                                 (new-mt-zombies)))
                                             (new-cons-zombies (new-zombie (new-posn -40 -23))
                                                               (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                                 (new-cons-zombies (new-zombie (new-posn -50 23))
                                                                                                   (new-cons-zombies (new-zombie (new-posn 2 3))
                                                                                                                     (new-mt-zombies)))))))
                       "world 2 no arg test")
   ))
   
(run-tests new-world-tests)
(run-tests w0-tests)
(run-tests world/no-args-tests)

;; ------------------------------------------------------
;; MAIN

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
