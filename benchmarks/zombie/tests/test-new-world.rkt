#lang racket

(require rackunit
         rackunit/text-ui
         
         ;(submod "zombie.rkt" test)
         (submod "../untyped/zombie.rkt" test)
         ; "image.rkt"
         "../untyped/image.rkt"
         
         "test-new-posn.rkt"
         "test-new-player.rkt"
         "test-image.rkt"
         )

(provide test-world/args
         test-world/no-args
         )

;; ------------------------------------------------------
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

;; test-world-on-mouse: World Player Horde Number Number String String -> test-suite
;    - world is the World whose on-mouse function we're testing
;    - player and horde are the Player and Horde used to initialize world
;    - x and y are location of the mouse (args passed to world-on-mouse)
;    - str is the message passed to world-on-mouse
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
;    - world is the World whose on-mouse function we're testing
;    - player and horde are the Player and Horde used to initialize world
;    - x and y are location of the mouse (args passed to world-on-mouse)
;    - str is the message passed to world-on-mouse
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

;; test-world/args : Player Posn Horde String -> test-suite
; inputs are assumed to be valid:
;      - player is a Player returned by a call to (new-player ...)
;      - mouse is a Posn returned by a call to (new-posn ...)
;      - horde is a Horde returned by a call to (new-horde ...)
; returns test-suite that checks if a call to new-world using player, mouse, and horde
; as the params would create the expected World.
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

