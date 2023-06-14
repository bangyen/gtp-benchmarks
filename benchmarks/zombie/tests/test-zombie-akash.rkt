#lang racket

(require rackunit
         rackunit/text-ui
         (submod "zombie.rkt" test)
         "image.rkt")



;; tests for new-posn
(define new-posn-test-suite
  (test-suite
   "test for new-posn"
   ;; (0,0)
   ;; posn-x
   (check-eq? ((posn-x (new-posn 0 0))) 0)
   ;; posn-y
   (check-eq? ((posn-y (new-posn 0 0))) 0)
   ;; posn-posn
   (check-eq? ((posn-x ((posn-posn (new-posn 0 0))))) 0)
   (check-eq? ((posn-y ((posn-posn (new-posn 0 0))))) 0)
   ;; move-toward/speed
   ;;; move the position towards a point (500, 400) with speed 5
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 500 400) 5)))
              5)
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 500 400) 5)))
              0)
   ;; toward (400, 500)
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 400 500) 5)))
              0)
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 400 500) 5)))
              5)
   ;;; move the position towards a point (1, 5) with speed 100000
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 1 5) 100000)))
              5)
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 1 5) 100000)))
              0)
   ;; move the position towards a point (5, 1) with speed 100000
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 5 1) 100000)))
              5)
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 0 0))
                        (new-posn 5 1) 100000)))
              0)
   ;; move
   ;;; move the position by (50, 20)
   (check-eq? ((posn-x ((posn-move (new-posn 0 0)) 50 20)))
              50)
   (check-eq? ((posn-y ((posn-move (new-posn 0 0)) 50 20)))
              20)
   ;; draw-on/image
   (check-true (image? ((posn-draw-on/image (new-posn 0 0))
                        (circle 5 "solid" "green") (empty-scene 50 50))))
   ;; dist between 0,0 and 5000, -100
   (check-within ((posn-dist (new-posn 0 0)) (new-posn 5000 -100))
                 5000.99990002
                 0.0001)
   ;; (400, 500)
   ;; posn-x
   (check-eq? ((posn-x (new-posn 400 500))) 400)
   ;; posn-y
   (check-eq? ((posn-y (new-posn 400 500))) 500)
   ;; posn-posn
   (check-eq? ((posn-x ((posn-posn (new-posn 400 500))))) 400)
   (check-eq? ((posn-y ((posn-posn (new-posn 400 500))))) 500)
   ;; move-toward/speed
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 400 500))
                        (new-posn 1000 400) 5)))
              405)
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 400 500))
                        (new-posn 1000 400) 5)))
              500)
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 400 500))
                        (new-posn -400 1500) 5)))
              400)
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 400 500))
                        (new-posn -400 1500) 5)))
              505)
   (check-eq? ((posn-x ((posn-move-toward/speed (new-posn 400 500))
                        (new-posn 1 5) 100000)))
              400)
   (check-eq? ((posn-y ((posn-move-toward/speed (new-posn 400 500))
                        (new-posn 1 5) 100000)))
              5)
   ;; move
   (check-eq? ((posn-x ((posn-move (new-posn 0 0)) 50 20)))
              50)
   (check-eq? ((posn-y ((posn-move (new-posn 0 0)) 50 20)))
              20)
   ;; draw-on/image (unnecessary)
   ;; dist
   (check-within ((posn-dist (new-posn 400 500)) (new-posn 5000 -100))
                 4638.9654018973
                 0.0001)
   
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