#lang racket

(require rackunit
         rackunit/text-ui
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         "test-zombie-akash.rkt" ; contains test-posn
         )

(provide algo-player-move-toward
         test-player
         )

;; -----------------------------------------
;; test new-player

; placeholder - checks that a posn is a posn
(define (test-posn posn-fxn x y msg)
  (test-suite
   msg
   (check < 2 3)
  ))

; placeholder - checks if two posns are equal
; posn-1 and posn-2 must be the function type returned by (new-posn ...)
(define (posn-equal? posn-1 posn-2)
  (and (= ((posn-x posn-1))
          ((posn-x posn-2)))
       (= ((posn-y posn-1))
          ((posn-y posn-2))))
  )

; returns new posn with the new location of player
(define (algo-player-move-toward x1 y1 x2 y2)
  ; x1 y1 is current location
  ; x2 y2 is where move towards
  (define xmove (- x2 x1))
  (define ymove (- y2 y1))
  (println xmove)
  (println ymove)
  (define dist (min PLAYER-SPEED
                    (max (abs xmove)
                         (abs ymove))))
  (println dist)
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
;; test-player: (-> player) (-> posn) string -> test-suite
;; returns a test suite with name msg with many checks that test whether
;; player-fxn is a valid player.
;; we will assume posn-fxn is a valid posn
(define (test-player player-fxn posn-fxn msg)
  (define player (player-fxn))
  (define posn (posn-fxn))
  (define x ((posn-x posn)))
  (define y ((posn-y posn)))
  
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
    (check-true (posn-equal? (algo-player-move-toward x y 1 2) ;expected
                             ((player-posn ((player-move-toward player) (new-posn 1 2)))))) ; actual
    (check-true (posn-equal? (algo-player-move-toward x y 0 0)
                             ((player-posn ((player-move-toward player) (new-posn 0 0))))))
    (check-true (posn-equal? (algo-player-move-toward x y 100 150)
                             ((player-posn ((player-move-toward player) (new-posn 100 150))))))
    (check-true (posn-equal? (algo-player-move-toward x y -5 20)
                             ((player-posn ((player-move-toward player) (new-posn -5 20))))))
    (check-true (posn-equal? (algo-player-move-toward x y 25 16)
                             ((player-posn ((player-move-toward player) (new-posn 25 16))))))
    (check-true (posn-equal? (algo-player-move-toward x y -5 -5)
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
((posn-x (algo-player-move-toward 5 0 1 2)))

;(player-move-toward test-player)
;(player-draw-on test-player)

(run-tests new-player-tests)

