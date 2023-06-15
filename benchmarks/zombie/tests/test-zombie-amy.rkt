#lang racket

(require rackunit
         rackunit/text-ui
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         )

;; -----------------------------------------
;; test new-player
#|
(new-posn x y)
- posn-x
- posn-y
- posn-posn
- posn-move-toward/speed
- posn-move
- posn-draw-on/image
- posn-dist

; (new-player posn)
(define (new-player p)
  (lambda (msg)
   (cond
    [(equal? msg 'posn)
     
     ; (player-posn player)
     (cons 'posn (lambda () p))]
    
    [(equal? msg 'move-toward)
     
     ; (player-move-toward player)
    (cons 'move-toward
     (lambda (q)
     (new-player ((posn-move-toward/speed p) q PLAYER-SPEED))))]
    
    [(equal? msg 'draw-on)
     
     ; (player-draw-on player)
    (cons 'draw-on
     (lambda (scn)
     ((posn-draw-on/image p) PLAYER-IMG scn)))]
    
    [else (error 'player "unknown message")])))
|#

; checks if posn is a posn
(define (check-posn posn msg)
  ...
  )
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
    "its param is a posn"
    (check < 1 2)
    )
   (test-suite
    "returns a fxn that takes 1 arg"
    (check < 1 2)
    )
   (test-suite
    "player-posn"
    (check < 1 2)
    )
   (test-suite
    "move-toward"
    (check < 1 2)
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

(define test-posn (new-posn 3 4))
(define test-player (new-player test-posn))
(player-posn test-player) ; fxn
((player-posn test-player)) ; fxn
(posn-x ((player-posn test-player)))
((posn-x ((player-posn test-player))))


;(player-move-toward test-player)
;(player-draw-on test-player)

(run-tests new-player-tests)

