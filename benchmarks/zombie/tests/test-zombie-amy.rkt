#lang racket

(require rackunit
         rackunit/text-ui
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         "test-zombie-akash.rkt" ; contains test-posn
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
|#
; (new-player posn)
#|
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

;; test-player: (-> player) (-> posn) string -> test-suite
;; returns a test suite with name msg with many checks that test whether
;; player-fxn is a valid player.
;; we will assume posn-fxn is a valid posn
(define (test-player player-fxn posn-fxn x y msg)
  
  
  (test-suite
   msg
   (test-suite
    "player-posn"
    ; check posn-fxn is a valid posn
    (test-posn posn-fxn 
    (check < 4 5)
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
    "player-posn"
    (check < 3 4)
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

(define t-posn (new-posn 3 4))
(define t-player (new-player t-posn))
(player-posn t-player) ; fxn that gives the posn
((player-posn t-player)) ; fxn that IS the posn
(posn-x ((player-posn t-player)))
((posn-x ((player-posn t-player))))
((posn-x t-posn))

(posn-equal? t-posn
             ((player-posn t-player)))

;(player-move-toward test-player)
;(player-draw-on test-player)

(run-tests new-player-tests)

