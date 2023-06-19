#lang racket

(require rackunit
         rackunit/text-ui
         (submod "../untyped/zombie.rkt" test)
         ;(submod "zombie.rkt" test)
         "../untyped/image.rkt"
         ;"image.rkt"
         "test-image.rkt"
         "test-new-posn.rkt"
         )

(provide test-posn
         posn-equal?
         algo-move-toward
         )

;; UTILITIES

;; zombie-equal?: Zombie Zombie -> Boolean
(define (zombie-equal? zombie-1 zombie-2)
  (posn-equal? ((zombie-posn zombie-1))
               ((zombie-posn zombie-2))))
;; tests for zombie-equal? 
(check-true (zombie-equal? (new-zombie (new-posn 0 0))
                           (new-zombie (new-posn 0 0))))
(check-false (zombie-equal? (new-zombie (new-posn 0 1))
                           (new-zombie (new-posn 0 0))))

; algo-move-toward-for-zombie - takes a posn and a zombie, and returns a new zombie
; Zombie Posn -> Zombie
; from-zombie: the original zombie
; to-posn: the posn the original zombie is moving towards
(define (algo-move-toward-for-zombie from-zombie to-posn)
  ;; get from-posn
  (define from-posn ((zombie-posn from-zombie)))
  ;; define speed
  (define speed ZOMBIE-SPEED)
  ;; create the new zombie
  (new-zombie (algo-move-toward from-posn to-posn speed)))
;; test zombie-algo-move-toward
(check-true (zombie-equal? (algo-move-toward-for-zombie (new-zombie (new-posn 0 0))
                                                  (new-posn 50 60))
                           (new-zombie (new-posn 0 ZOMBIE-SPEED))))
(check-false (zombie-equal? (algo-move-toward-for-zombie (new-zombie (new-posn 0 0))
                                                        (new-posn 40 30))
                           (new-zombie (new-posn 0 ZOMBIE-SPEED))))

;; is-zombie-really-touching?: Zombie Posn -> Boolean
(define (is-zombie-really-touching? z p)
  ;; zombie's posn
  (define z-posn ((zombie-posn z)))
  ;; dist
  (<= ((posn-dist p) z-posn) ZOMBIE-RADIUS))
;; tests for is-zombie-really-touching?
(check-true (is-zombie-really-touching? (new-zombie (new-posn 0 0)) 
                                        (new-posn ZOMBIE-RADIUS 0)))
(check-false (is-zombie-really-touching? (new-zombie (new-posn 0 0))
                                         (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS)))

;; test-zombie: (-> Zombie) Real Real String -> Void
;; generates and runs a test suite with message msg, 
;; testing the result of z-fxn against the Zombie 
;; created with position (x, y)
(define (test-zombie z-fxn x y msg)
  (define result (z-fxn))
  (define posn-touching-z ((posn-move ((zombie-posn result))) ZOMBIE-RADIUS 0))
  (define posn-not-touching-z ((posn-move ((zombie-posn result))) ZOMBIE-RADIUS ZOMBIE-RADIUS))
  (test-suite
   msg
   ;; tests
   ;; posn
   (posn-equal? ((zombie-posn result))
                (new-posn x y))
   ;; draw-on/color
   (place-image-check (lambda () ((zombie-draw-on/color result) "red" MT-SCENE))
                      (circle ZOMBIE-RADIUS "solid" "red")
                      x
                      y
                      MT-SCENE
                      "")
   ;; touching?
   (check-eq? (is-zombie-really-touching? result posn-touching-z)
              ((zombie-touching? result) posn-touching-z))
   (check-eq? (is-zombie-really-touching? result posn-not-touching-z)
              ((zombie-touching? result) posn-not-touching-z))           
   ;; move-toward
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn 0 0))
                              ((zombie-move-toward result) (new-posn 0 0))))
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn -300000 -40000))
                              ((zombie-move-toward result) (new-posn -300000 -40000))))
   (check-true (zombie-equal? (algo-move-toward-for-zombie result (new-posn 30000 30000))
                              ((zombie-move-toward result) (new-posn 30000 30000))))
   ))


(define testing-test-zombie (test-zombie (lambda () (new-zombie (new-posn 0 0)))
                                         0 0 ""))
(run-tests testing-test-zombie)

;; TESTS

;; tests for new-zombie
(define new-zombie-test-suite
  (test-suite
    "test for new-zombie"
    ;; tests using test-zombie
    (test-zombie (lambda () (new-zombie (new-posn 0 0))) 0 0 "")
    (test-zombie (lambda () (new-zombie (new-posn 500 300))) 500 300 "")
    (test-zombie (lambda () (new-zombie (new-posn -100 100))) -100 100 "")
    (test-zombie (lambda () (new-zombie (new-posn -30000 -500))) -30000 -500 "")
    (test-zombie (lambda () (new-zombie (new-posn 500 -500))) 500 -500 "")
    ;; test for wrong inputs
    (check-exn exn:fail? (lambda () ((new-zombie ""))))
    ;; (check-exn exn:fail? (lambda () ((zombie-posn (new-posn 0 0)))))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color 5) 5 MT-SCENE)))
    (check-exn exn:fail? (lambda () ((zombie-draw-on/color "red") "red" 5)))
    (check-exn exn:fail? (lambda () ((zombie-touching? (new-posn 5 5)))))
    (check-exn exn:fail? (lambda () ((zombie-move-toward (new-zombie (new-posn 0 0))))))
    ))

;; run test suites
(run-tests new-zombie-test-suite)