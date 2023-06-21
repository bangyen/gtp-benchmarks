#lang racket

(require rackunit
         rackunit/text-ui
         
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         ; "image.rkt"
         "../untyped/image.rkt"
         
         "test-new-posn.rkt"
         "test-new-player.rkt"
         "test-new-zombie.rkt"
         "test-image.rkt"
         )

(provide test-world/args
         test-world/no-args
         )

;; ------------------------------------------------------
;; HELPERS

;; test-world/args : Player Posn Horde String -> test-suite
; inputs are assumed to be valid:
;      - player is a Player returned by a call to (new-player ...)
;      - mouse is a Posn returned by a call to (new-posn ...)
;      - horde is a Horde returned by a call to (new-horde ...)
; returns test-suite that checks if a call to new-world using player, mouse, and horde
; as the params would create the expected World.
(define (test-world/args player mouse horde msg)
  ; create a World using provided args
  (define world (new-world player mouse horde))
  ; check if this World is as expected
  (test-suite
   msg
   (test-suite
    "world-on-mouse" ; Real Real String -> World
    (check-true (procedure? (world-on-mouse world)))
    (check-equal? (procedure-arity (world-on-mouse world)) 3)
    (check-true (procedure? ((world-on-mouse world) 6 10 "cat"))) ; World
    (check-equal? (procedure-arity ((world-on-mouse world) 6 10 "cat")) 1)
    (check-true (procedure? (world-on-mouse ((world-on-mouse world) 6 10 "cat"))))
    (check-equal? (procedure-arity (world-on-mouse ((world-on-mouse world) 6 10 "cat"))) 3)

    ;(new-world .. .. .. ) same as (world-on-mouse ... ... ..)
    
    )
   (test-suite
    "world-on-tick"
    (check < 3 4)
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
    "world-stop-when"
    (check < 6 7)
    )
   )
  )

;; test-world/no-args : World -> test-suite
;      - world is a World returned by (new-world ...)
; a lighter, less exhaustive test. while test-world/args allows you to check that
; the world-... fxns return both the correct Type AND value, test-world/noargs only
; checks that the world-... fxns return the correct Type.
; this should only be used to check the validity of a World when you don't know
; what was passed in as the args to (new-world ...)
(define (test-world/no-args world msg)
  (test-suite
   msg
   (test-suite
    "world-on-mouse"
    (check-true (procedure? (world-on-mouse world)))
    (check-equal? (procedure-arity (world-on-mouse world)) 3)
    (check < 1 2)
    )
   (test-suite
    "world-on-tick"
    (check < 3 4)
    )
   (test-suite
    "world-to-draw"
    (check < 5 6)
    )
   (test-suite
    "world-stop-when"
    (check < 6 7)
    )
   )
  )
  


;; --------------------------------------------------------
;; TESTS

(define new-world-tests
  (test-suite
   "new-world-tests"
   #:before (lambda () (display "START new-world-tests\n"))
   #:after  (lambda () (display "FINISH new-world-tests\n"))
   (test-suite
    "accepts 3 args only"
    (check < 1 2)
    )
   (test-suite
    "player, posn, horde"
    (check < 3 4)
    )
   (test-suite
    "produces valid world"
    (test-world/args (new-player (new-posn 5 6))
                     (new-posn 0 9)
                     (new-horde (new-cons-zombies (new-zombie (new-posn 10 20))
                                                  (new-cons-zombies (new-zombie (new-posn 15 16))
                                                                    (new-mt-zombies)))
                                (new-cons-zombies (new-zombie (new-posn 30 31))
                                                  (new-mt-zombies)))
                     "world 1 test")
    (check < 6 7)
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
   
(run-tests new-world-tests)
(run-tests w0-tests)

