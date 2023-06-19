#lang racket

(require rackunit
         rackunit/text-ui
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         "test-zombie-akash.rkt" ; contains test-posn
         )
; test-zombie-akash
; (posn-equal? posn-1 posn-2)
;     - posn-1 and posn-2 are posns returned by (new-posn ...)
;     - returns T if equal, F if not
; (test-posn posn-fxn x y msg)
;     - posn-fxn is a lambda fxn that runs (new-posn... )
;     - x and y are the x and y used in call to (new-posn... )
;     - returns test suite
; (algo-move-toward from-posn to-posn speed)
; from-posn and to-posn are posns returned by (new-posn ...)
; runs a copy of the posn-move-toward/speed algo with from-posn moving towards to-posn.
; returns a posn created by (new-posn ...) with the new location of from-posn

;; -----------------------------------------
;; test new-player

;; test-player: (-> player) (-> posn) string -> test-suite
;; (test-player player-fxn posn-fxn msg)
;      - player-fxn is a lambda call to (new-player ...)
;      - posn-fxn is a lambda call to (new-posn ...)
;  the posn passed as posn-fxn must be the posn that the player is initialized with.
;; returns a test suite with name msg
(define (test-player player-fxn posn-fxn msg)
  ; assume posn-fxn gives us a valid posn. we will not run (test-posn ...) on it.
  ;; SETUP
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
    (check < 1 2)
    )
   (test-suite
    "other msgs"
    (check < 1 2)
    )
   ))

;(new-player posn)
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
    )   
   (test-suite
    "valid instances"
    (test-player (lambda () (new-player (new-posn 5 0)))
                 (lambda () (new-posn 5 0))
                 "player at (5,0)")
    
    )
   ))

(define t-posn (new-posn 3 4))
(define t-player (new-player t-posn))
(player-posn t-player) ; fxn that gives the posn
((player-posn t-player)) ; fxn that IS the posn
(posn-x ((player-posn t-player)))
((posn-x ((player-posn t-player))))
((posn-x t-posn))

(posn-equal? t-posn
             ((player-posn t-player)))

((player-move-toward (new-player (new-posn 5 0))) (new-posn 1 2))
((posn-x ((player-posn ((player-move-toward (new-player (new-posn 5 0))) (new-posn 1 2))))))
((posn-x (algo-move-toward (new-posn 5 0) (new-posn 1 2) PLAYER-SPEED)))
(posn-equal? (algo-move-toward (new-posn 5 0) (new-posn 1 2) PLAYER-SPEED)
             ((player-posn ((player-move-toward (new-player (new-posn 5 0))) (new-posn 1 2)))))

;(player-move-toward test-player)
;(player-draw-on test-player)

(run-tests new-player-tests)

