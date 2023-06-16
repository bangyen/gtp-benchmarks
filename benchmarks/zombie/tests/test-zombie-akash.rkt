#lang racket

(require rackunit
         rackunit/text-ui
         (submod "../untyped/zombie.rkt" test)
         ;(submod "zombie.rkt" test)
         "../untyped/image.rkt"
         ;"image.rkt"
         ; "test-image.rkt"
         )

; posn-1 and posn-2 must be the function type returned by (new-posn ...)
(define (posn-equal? posn-1 posn-2)
  (and (= ((posn-x posn-1))
          ((posn-x posn-2)))
       (= ((posn-y posn-1))
          ((posn-y posn-2)))))

; (test-posn (lambda () (new-posn 0 1)) 0 1) -> test-suite
(define (test-posn posn-fxn x y msg)
  (define result (posn-fxn))
  (define test-posn-1 (new-posn 0 0))
  ; (define test-posn-2 (new-posn 20 50))
  
  (define test-move-toward-1 ((posn-move-toward/speed result)
                              test-posn-1 1))
  (define test-move-toward-2 ((posn-move-toward/speed result)
                              test-posn-1 (+ (abs x) (abs y))))
  ; (define test-move-toward-3 ((posn-move-toward/speed result)
  ;                             test-posn-2 1))
  ; (define test-move-toward-4 ((posn-move-toward/speed result)
  ;                             test-posn-2 10000))

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
   ; (check-eq? ((posn-x test-move-toward-1))
   ;            (+ x 1))
   ; (check-eq? ((posn-y test-move-toward-1))
   ;            y)
   ; (check-eq? ((posn-x test-move-toward-2))
   ;            50)
   ; (check-eq? ((posn-y test-move-toward-2))
   ;            y)
   ; (check-eq? ((posn-x test-move-toward-3))
   ;            x)
   ; (check-eq? ((posn-y test-move-toward-3))
   ;            (+ y 1))
   ; (check-eq? ((posn-x test-move-toward-4))
   ;            x)
   ; (check-eq? ((posn-y test-move-toward-4))
   ;            50)
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
   (check-within ((posn-dist result) (new-posn 0 0))
                 (sqrt (+ (* x x) (* y y)))
                 0.0001)
   ))


;; tests for test-posn
(define testing-test-posn
  ; testing my helper fxn - amy
  (test-posn (lambda () (new-posn 1 2)) 1 2 "testing test-posn"))

(run-tests testing-test-posn)



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