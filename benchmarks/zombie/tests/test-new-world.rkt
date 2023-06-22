#lang racket

(require rackunit
         rackunit/text-ui
         
         (submod "zombie.rkt" test)
         ; (submod "../untyped/zombie.rkt" test)
         "image.rkt"
         ; "../untyped/image.rkt"
         
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

;; test-world-on-tick: World Player Horde Posn String -> test-suite
;    - world is a World whose on-tick function we're testing
;    - player and horde are the Player and Horde used to initialize world
;    - mouse is the location of the mouse
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
;    - let world be any World returned by (new-world ...)
;    - wom is the World returned by ((world-on-mouse world) ...)
;    - player and horde are the Player and Horde used to initialize world
;    - x, y, and str are args passed to the call to ((world-on-mouse world) ...)
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

;; test-world/args : World Player Posn Horde String -> test-suite
; inputs are assumed to be valid:
;      - world is a World returned by a call to (new-world ...)
;      - player is a Player returned by a call to (new-player ...)
;      - mouse is a Posn returned by a call to (new-posn ...)
;      - horde is a Horde returned by a call to (new-horde ...)
; returns test-suite that checks if a World is a valid world
; based on the args passed as inputs when initializing this World.
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

;; test-world-on-mouse/noargs: World World Number Number String String -> test-suite
;    - world is a World returned by (new-world ...)
;    - wom is the World returned by ((world-on-mouse world) ...)
;    - x, y, and str are the args passed to the call to ((world-on-mouse world) ...)
; assumes world's on-mouse fxn has correct arity and param types
(define (test-world-on-mouse/noargs world wom x y str msg)
  (define player (new-player (new-posn (second (image-impl ((world-to-draw world))))
                                       (third (image-impl ((world-to-draw world)))))))
  (define mouse (if (equal? str "leave")
                    ((player-posn player))
                    (new-posn x y)))
  (test-suite
   msg
   ; wom is a World type
   (check-true (procedure? wom))
   (check-equal? (procedure-arity wom) 1)
   (check-true (procedure? (world-on-tick wom)))
   (check-equal? (procedure-arity (world-on-tick wom)) 0)
   (check-true (procedure? (world-to-draw wom)))
   (check-equal? (procedure-arity (world-to-draw wom)) 0)
   ; wom looks the exact same as world
   (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw wom)))))
   (check-equal? (second (image-impl ((world-to-draw world))))
                 (second (image-impl ((world-to-draw wom)))))
   (check-equal? (third (image-impl ((world-to-draw world))))
                 (third (image-impl ((world-to-draw wom)))))
   (test-image-equal (fourth (image-impl ((world-to-draw world))))
                     (fourth (image-impl ((world-to-draw wom)))))
   ; test ticks
   (test-world-on-tick/mouse wom mouse msg)
   ))

;; test-world-on-tick/mouse: World Posn String -> test-suite
;    - world is a World returned by (new-world ...)
;    - mouse is the current mouse location
; assumes world's on-tick fxn has correct arity and param types
(define (test-world-on-tick/mouse wom mouse msg)
  (define player (new-player (new-posn (second (image-impl ((world-to-draw wom))))
                                       (third (image-impl ((world-to-draw wom)))))))
  (define (test-n-ticks n)
    (define player-n (player-ticks player mouse n))
    (define world-n (world-ticks wom n))
    (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw world-n)))))
    (check-equal? ((posn-x ((player-posn player-n))))
                  (second (image-impl ((world-to-draw world-n)))))
    (check-equal? ((posn-y ((player-posn player-n))))
                  (third (image-impl ((world-to-draw world-n)))))
    (check-true (image? (fourth (image-impl ((world-to-draw world-n))))))
  )
  (test-suite
   msg
   (test-n-ticks 1)
   (test-n-ticks 2)
   (test-n-ticks 3)
   (test-n-ticks 6)
   (test-n-ticks 10)
   ))

;; test-world/no-args : World -> test-suite
;      - world is a World returned by (new-world ...)
; a lighter, less exhaustive test. while test-world/args allows you to check that
; the world-... fxns return both the correct Type AND value, test-world/noargs only
; checks that the world-... fxns return the correct Type.
; this should only be used to check the validity of a World when you don't know
; what values were used to initalize the World.
(define (test-world/no-args world msg)
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
    (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw world)))))
    (check-true (real? (second (image-impl ((world-to-draw world))))))
    (check-true (real? (third (image-impl ((world-to-draw world))))))
    (check-true (image? (fourth (image-impl ((world-to-draw world))))))
    )
   (test-suite
    "world-on-tick"
    (check-true (procedure? (world-on-tick world)))
    (check-equal? (procedure-arity (world-on-tick world)) 0)
    ; world-on-tick produces a world, which is a procedure
    (check-true (procedure? ((world-on-tick world))))
    (check-equal? (procedure-arity ((world-on-tick world))) 1)
    ; new world looks right
    (test-image-equal PLAYER-IMG (first (image-impl ((world-to-draw ((world-on-tick world)))))))
    (check-true (real? (second (image-impl ((world-to-draw ((world-on-tick world))))))))
    (check-true (real? (third (image-impl ((world-to-draw ((world-on-tick world))))))))
    (check-true (image? (fourth (image-impl ((world-to-draw ((world-on-tick world))))))))
    )
   (test-suite
    "world-on-mouse"
    (check-true (procedure? (world-on-mouse world)))
    (check-equal? (procedure-arity (world-on-mouse world)) 3)
    ; world-on-mouse produces a world, which is a procedure
    (check-true (procedure? ((world-on-mouse world) 1 2 "")))
    (check-equal? (procedure-arity ((world-on-mouse world) 1 2 "")) 1)
    ; new world looks right - same player and horde but new mouse, which changes tick behavior.
    (test-world-on-mouse/noargs world ((world-on-mouse world) 1 2 "")
                                1 2 ""
                                "1 2 blank")
    (test-world-on-mouse/noargs world ((world-on-mouse world) -1 -8 "")
                                -1 -8 ""
                                "-1 -8 blank")
    (test-world-on-mouse/noargs world ((world-on-mouse world) 0 -120 "")
                                0 -120 ""
                                "0 -120 blank")
    (test-world-on-mouse/noargs world ((world-on-mouse world) 57 6 "leave")
                                57 6 "leave"
                                "57 6 leave")
    (test-world-on-mouse/noargs world ((world-on-mouse world) -2 49 "leave")
                                -2 49 "leave"
                                "-2 49 leave")
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
    (check-true (procedure? new-world))
    (check-equal? (procedure-arity new-world) 3)
    )
   (test-suite
    "player, posn, horde"
    ;; TODO: how do you test restrictions on inputs? try running stuff and doesn't work?
    ;; maybe helper for this
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
    (test-world/args (new-world (new-player (new-posn 17 600))
                                (new-posn 134 103)
                                (new-horde (new-cons-zombies (new-zombie (new-posn 18 590))
                                                             (new-cons-zombies (new-zombie (new-posn 15 160))
                                                                               (new-mt-zombies)))
                                           (new-cons-zombies (new-zombie (new-posn 0 0))
                                                             (new-cons-zombies (new-zombie (new-posn 400 16))
                                                                               (new-mt-zombies)))))
                     (new-player (new-posn 17 600))
                     (new-posn 134 103)
                     (new-horde (new-cons-zombies (new-zombie (new-posn 18 590))
                                                             (new-cons-zombies (new-zombie (new-posn 15 160))
                                                                               (new-mt-zombies)))
                                           (new-cons-zombies (new-zombie (new-posn 0 0))
                                                             (new-cons-zombies (new-zombie (new-posn 400 16))
                                                                               (new-mt-zombies))))
                     "world 2 test")
    (test-world/args (new-world (new-player (new-posn 1 3))
                                (new-posn 0 6)
                                (new-horde (new-cons-zombies (new-zombie (new-posn 0 5))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies)))
                     (new-player (new-posn 1 3))
                     (new-posn 0 6)
                     (new-horde (new-cons-zombies (new-zombie (new-posn 0 5))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies))
                     "world 3 test")
    (test-world/args (new-world (new-player (new-posn -50 23))
                                (new-posn -50 23)
                                (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies)))
                     (new-player (new-posn -50 23))
                     (new-posn -50 23)
                     (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                  (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                    (new-mt-zombies)))
                                (new-mt-zombies))
                     "world 4 test")
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

; more tests of test-world/no-args
(define world/no-args-tests
  (test-suite
   "test-world/no-args tests"
   #:before (lambda () (display "START test-world/no-args tests\n"))
   #:after  (lambda () (display "FINISH test-world/no-args tests\n"))
   (test-world/no-args (new-world (new-player (new-posn -50 23))
                                (new-posn -50 23)
                                (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                             (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                               (new-mt-zombies)))
                                           (new-mt-zombies)))
                       "world 1 no arg test")
   (test-world/no-args (new-world (new-player (new-posn -50 23))
                                  (new-posn -50 23)
                                  (new-horde (new-cons-zombies (new-zombie (new-posn -50 23))
                                                               (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                                 (new-mt-zombies)))
                                             (new-cons-zombies (new-zombie (new-posn -40 -23))
                                                               (new-cons-zombies (new-zombie (new-posn 3 3))
                                                                                 (new-cons-zombies (new-zombie (new-posn -50 23))
                                                                                                   (new-cons-zombies (new-zombie (new-posn 2 3))
                                                                                                                     (new-mt-zombies)))))))
                       "world 2 no arg test")
   ))
   
(run-tests new-world-tests)
(run-tests w0-tests)
(run-tests world/no-args-tests)
