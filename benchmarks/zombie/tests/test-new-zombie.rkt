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

;; place-zombie-image-check Zombie string? scene -> void
;; checks whether a given zombie z draws properly with
;; a given color c and scene s 
(define (place-zombie-image-check z c s)
  (define z-posn ((zombie-posn z)))
  (place-image-check (lambda () ((zombie-draw-on/color z) c s))
                      (circle ZOMBIE-RADIUS "solid" c)
                      ((posn-x z-posn))
                      ((posn-y z-posn))
                      s
                      ""))

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
  ;  (place-image-check (lambda () ((zombie-draw-on/color result) "red" MT-SCENE))
  ;                     (circle ZOMBIE-RADIUS "solid" "red")
  ;                     x
  ;                     y
  ;                     MT-SCENE
  ;                     "")
  (place-zombie-image-check result "red" MT-SCENE)

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

;; place-images-recursively: List (Any -> Image) string? Image -> Image
;; helper for place-zombies-image-check
(define (place-images-recursively l i-fxn x-fxn y-fxn s)
  (if (empty? l)
      s
      (local [(define obj (first l))]
        (place-image (i-fxn obj)
                    (x-fxn obj)
                    (y-fxn obj)
                    (place-images-recursively (rest l)
                                              i-fxn
                                              x-fxn
                                              y-fxn
                                              s)))))
;; place-zombies-image-check: Zombies (listof zombie) string image -> void
;; checks whether a list of zombies zs (should incl. mt-zombie) 
;; draws properly with color c and scene s
(define (place-zombies-image-check actual-zs zs c s)
  (define img (place-images-recursively zs (lambda (z)
                                              (circle ZOMBIE-RADIUS "solid" c))
                                           (lambda (z)
                                              ((posn-x ((zombie-posn z)))))
                                           (lambda (z)
                                              ((posn-y ((zombie-posn z)))))
                                           s))
  (test-image-equal ((zombies-draw-on/color actual-zs) c s)
                    img))

;; zombies-really-touching?: (listof zombie) posn -> Boolean 
;; discerns whether a list of zombie zs is touching a
;; posn p
(define (zombies-really-touching? zs p)
  (cond [(empty? zs)
         #f]
        [(is-zombie-really-touching? (first zs) p)
         #t]
        [else
         (zombies-really-touching? (rest zs) p)]))


;; helper for kill-all-zombies
(define (convert-list-to-cons-zombie list-zs)
  (if (empty? list-zs)
      (new-mt-zombies)
      (new-cons-zombies (first list-zs)
                (convert-list-to-cons-zombie (rest list-zs)))))

;; kill-all-zombies-list: (listof zombie) (listof zombie) -> (listof (listof zombie))
;; kills all the zombies in undead-zombies if they
;; come in contact with dead-zombies without using 
;; higher-order recursive functions to test zombie 
;; functions properly
(define (kill-all-zombies undead-zombies dead-zombies)
  (cond [(empty? undead-zombies)
         (list undead-zombies dead-zombies)]
        [(or (zombies-really-touching? (rest undead-zombies) 
                                       ((zombie-posn (first undead-zombies))))
             (zombies-really-touching? dead-zombies
                                       ((zombie-posn (first undead-zombies)))))
         (kill-all-zombies (rest undead-zombies) (cons (first undead-zombies) dead-zombies))]
        [else
         (define res (kill-all-zombies (rest undead-zombies) dead-zombies))
         (list (cons (first undead-zombies)
                     (first res))
               (second res))]))

;; test-new-mt-zombies
(define (test-new-mt-zombies z-fxn msg)
  (define result (z-fxn))
  (define real-zombie-list 
          (new-cons-zombies (new-zombie (new-posn 0 0))
                            (new-cons-zombies (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                                              (new-cons-zombies (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))
                                                                (new-mt-zombies)))))
  (define fake-zombie-list '((new-zombie (new-posn 0 0)) (new-zombie (new-posn ZOMBIE-RADIUS ZOMBIE-RADIUS))
                             (new-zombie (new-posn (- 0 ZOMBIE-RADIUS) (- 0 ZOMBIE-RADIUS)))))
  (define h-list (kill-all-zombies '() fake-zombie-list))
  (define h (new-horde (convert-list-to-cons-zombie (first h-list))
                      (convert-list-to-cons-zombie (second h-list))))
  (test-suite
   msg
   ;; instance check
   (check-equal? result (new-mt-zombies))
   ;; move-toward check
   (check-equal? ((zombies-move-toward result) (new-posn 0 0)) (new-mt-zombies))
   ;; draw-on/color check
   (place-zombies-image-check result '() "red" MT-SCENE)
   ;; touching?
   (check-equal? (zombies-really-touching? '() (new-posn 0 0))
                ((zombies-touching? result) (new-posn 0 0)))
   ;; kill-all
   ;; kill-all-zombies where result = undead
   (check-equal? h ((zombies-kill-all result) real-zombie-list))
   ;; kill-all-zombies where result = dead
   ))

(run-tests (test-new-mt-zombies (lambda () (new-mt-zombies)) ""))

   

;; test-new-cons-zombies

;; test-new-horde

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
    ;; wrong message
    ))

;; run test suites
(run-tests new-zombie-test-suite)