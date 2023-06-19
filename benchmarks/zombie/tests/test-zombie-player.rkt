#lang racket

(require rackunit
         rackunit/text-ui
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         "../untyped/image.rkt"
         "test-zombie-akash.rkt"
         "test-image.rkt"
         )

(provide test-player-draw-on
         test-player
         )

;; ---------------------------------------------
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
                      "place-image-check")
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
    (test-player-draw-on player (image '(PLAYER-IMG 5 6 (empty-scene 10 20))) "player on img w player")
    (test-player-draw-on player (image '(ZOMBIE-IMG 5 6 (empty-scene 10 20))) "player on img w zombie")
    (test-player-draw-on player (image '(ZOMBIE-IMG 10 20 (image '(PLAYER-IMG 5 6 (empty-scene 10 20))))) "player on img w zombie and player")
    (test-player-draw-on player (circle 200 "big" "blue") "player on circle")
    (test-player-draw-on player (place-image (circle 20 "small" "pink") 2 1 (empty-scene 5 500))
                         "player on place-image")
    )
   ; Note: cannot test "else" case ("unknown message") because code won't compile
   ))

;; ------------------------------------------------
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

