#lang racket

(require rackunit
         rackunit/text-ui
         (submod "../untyped/zombie.rkt" test)
         ;; (submod "zombie.rkt" test)
         "../untyped/image.rkt"
         ;; "image.rkt"
         "test-image.rkt"
         )

(provide test-posn
         posn-equal?
         algo-move-toward
         )

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

  ;; (test-suite
   ;; msg
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
   )
;; )
;; tests for test-posn
;; (define testing-test-posn
;;   (test-posn (lambda () (new-posn 1 2)) 1 2 "testing test-posn"))
;; (run-tests testing-test-posn)


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
